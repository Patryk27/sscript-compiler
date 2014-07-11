(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit LLCompiler;

 Interface
 Uses Logging, HLCompiler, Expression, symdef, MixedValue, Classes, SysUtils, Variants, Opcodes, Tokens, Messages, List, Zipper, Stream;

 { TBytecodeVersion }
 Type TBytecodeVersion =
      Record
       Major, Minor: uint8;
      End;

 Const BytecodeVersion: TBytecodeVersion =
       (Major: 0; Minor: 5);

 { ELLCompilerException }
 Type ELLCompilerException = Class(Exception);

 { TBCLabel }
 Type TBCLabel =
      Record
       Name    : String;
       Position: uint32;

       isPublic, isFunction: Boolean;
       FunctionSymbol      : TSymbol; // if 'isFunction' equals 'true'
      End;

 { TBCAllocatedSymbol }
 Type TBCAllocatedSymbol =
      Record
       Name : String;
       Value: TExpressionNode;

       Position: uint32;
       Size    : uint8;
      End;

 { arrays }
 Type TBCLabelList           = specialize TList<TBCLabel>;
      TBCAllocatedSymbolList = specialize TList<TBCAllocatedSymbol>;

 { TCompiler }
 Type TCompiler =
      Class
       Public { fields }
        HeaderStream   : TStream;
        DebugDataStream: TStream;
        BytecodeStream : TStream;
        ReferenceStream: TStream;

        Compiler: HLCompiler.TCompiler;
        Bytecode: TWriter;

        LabelList      : TBCLabelList;
        AllocSymbolList: TBCAllocatedSymbolList;

       Private { methods }
        Function getLabelID(const Name: String): int32;
        Function CreateReference(const Name: String): uint32;
        Function AllocateGlobalVar(const Name: String): uint32;

        Procedure Preparse(const AllocateGlobalVars: Boolean);
        Procedure Parse(const ResolveReferences: Boolean);

       Public { methods }
        Constructor Create;
        Destructor Destroy; override;

        Procedure Compile(const fCompiler: HLCompiler.TCompiler; const SaveAs_SSM: Boolean);
       End;

 // primary types; order (those numbers) is important, as it is the same in the virtual machine
 Const PrimaryTypeNames: Array[0..6] of String = ('bool', 'char', 'int', 'float', 'string', 'any', 'void');
 Const TYPE_BOOL_id   = 0;
       TYPE_CHAR_id   = 1;
       TYPE_INT_id    = 2;
       TYPE_FLOAT_id  = 3;
       TYPE_STRING_id = 4;

       TYPE_ANY_id  = 5;
       TYPE_VOID_id = 6;

 Implementation
Uses SSMParser, BCDebug, CommandLine, Serialization;

(* TCompiler.getLabelID *)
Function TCompiler.getLabelID(const Name: String): int32;
Begin
 For Result := 0 To LabelList.Count-1 Do
 Begin
  if (LabelList[Result].Name = Name) Then
   Exit;
 End;

 Exit(-1);
End;

(* TCompiler.CreateReference *)
Function TCompiler.CreateReference(const Name: String): uint32;
Begin
 Result := ReferenceStream.Position;
 ReferenceStream.write_nullstring(Name);
End;

(* TCompiler.AllocateGlobalVar *)
Function TCompiler.AllocateGlobalVar(const Name: String): uint32;
Var Data       : TUnserializer;
    AllocSymbol: TBCAllocatedSymbol;

    Size : int32;
    Value: TExpressionNode;

    I, Pos: int32;
Begin
 Pos := 0

 // call()
 +1 +1 +8

 // stop()
 +1
 ;

 // allocate symbol after the last one
 For I := 0 To AllocSymbolList.Count-1 Do
 Begin
  Inc(Pos, AllocSymbolList[I].Size);

  if (AllocSymbolList[I].Name = Name) Then
   Exit(AllocSymbolList[I].Position);
 End;

 // fetch data from serialized form of the variable
 Data := TUnserializer.Create(Name);

 Try
  Size  := Data.getRoot[2].getInt;
  Value := TExpressionNode(Data.getRoot[4].getExpression(Compiler));

  if (Value <> nil) and (Value.getPredictedType = mvString) Then
   Inc(Size, 3); // count the '0xFF' special byte and the string length (uint16)
 Finally
  Data.Free;
 End;

 // prepare allocsymbol
 AllocSymbol.Name     := Name;
 AllocSymbol.Value    := Value;
 AllocSymbol.Position := Pos;
 AllocSymbol.Size     := Size;

 // insert it onto the list
 AllocSymbolList.Add(AllocSymbol);

 // return its position
 Exit(Pos);
End;

(* TCompiler.Preparse *)
Procedure TCompiler.Preparse(const AllocateGlobalVars: Boolean);
Var OpcodesLength: uint32 = 0;
    OpcodeSize   : uint32 = 0;
    IncSize      : Boolean;

    OpcodeID: int32;
    ArgID   : int8;
    Op      : PMOpcode = nil;
    Arg     : PMOpcodeArg = nil;

    Value, TmpValue: String;
    CharCode       : int32;
    Pnt            : Pointer;

    InsertedOpcodes: uint32 = 0;

    mNamespace: TNamespace;
    mSymbol   : TSymbol;

    BCLabel: TBCLabel;
    BCAlloc: TBCAllocatedSymbol;
    vValue : Variant;
Begin
 With Compiler do
 Begin
  if (OpcodeList.Count = 0) Then
   raise ELLCompilerException.Create('No opcodes to preparse!'); // unexpected to happen, but nonetheless...

  OpcodesLength := 0;

  // allocate global variables
  if (AllocateGlobalVars) Then
  Begin
   // allocate symbols #1
   For mNamespace in Compiler.NamespaceList Do
   Begin
    For mSymbol in mNamespace.getSymbolList Do
    Begin
     if (mSymbol.Typ = stVariable) Then
      AllocateGlobalVar(mSymbol.mVariable.LocationData.MemSymbolName);
    End;
   End;

   // allocate symbols #2
   {
    @Note: "Insert(2, ...)" because first two opcodes are (and have to remain) "call" and "stop"
   }
   if (AllocSymbolList.Count > 0) Then
   Begin
    DoNotStoreOpcodes := True;

    For BCAlloc in AllocSymbolList Do
    Begin
     if (BCAlloc.Value = nil) Then
     Begin
      Op := nil;

      Case BCAlloc.Size of
       1: Op := PutOpcode(o_char, [0]);
       8: Op := PutOpcode(o_int, [0]);

       else
        raise ELLCompilerException.CreateFmt('Invalid BCAlloc.Size: %d', [BCAlloc.Size]);
      End;

      if (Op <> nil) Then
      Begin
       OpcodeList.Insert(2+InsertedOpcodes, Op);
       Inc(InsertedOpcodes);
      End;
     End Else
     Begin
      vValue := TConstantExpressionNode(BCAlloc.Value).getValue;

      Case BCAlloc.Value.getPredictedType of
       mvBool : Op := PutOpcode(o_bool, [ord(Boolean(vValue))]);
       mvChar : Op := PutOpcode(o_char, [ord(Char(vValue))]);
       mvInt  : Op := PutOpcode(o_int, [int64(vValue)]);
       mvFloat: Op := PutOpcode(o_float, [Double(vValue)]);

       mvString:
       Begin
        OpcodeList.Insert(2+InsertedOpcodes, PutOpcode(o_char, [$FF]));
        Inc(InsertedOpcodes);
        Op := PutOpcode(o_string, [AnsiString(vValue)]);
       End;

       else
        raise ELLCompilerException.CreateFmt('Unknown expression type: %d', [ord(BCAlloc.Value.getPredictedType)]);
      End;

      OpcodeList.Insert(2+InsertedOpcodes, Op);
      Inc(InsertedOpcodes);
     End;
    End;

    DoNotStoreOpcodes := False;
   End;
  End;

  // parse opcodes and labels
  For OpcodeID := 0 To OpcodeList.Count Do
  Begin
   if (Op <> nil) Then // save the size of the *previous* opcode
   Begin
    Op^.OpcodeSize := OpcodeSize;
    Inc(OpcodesLength, OpcodeSize);
   End;

   if (OpcodeID = OpcodeList.Count) Then
    Break;

   Op            := OpcodeList[OpcodeID];
   Op^.OpcodePos := OpcodesLength;
   Op^.OpcodeID  := OpcodeID;

   OpcodeSize := 0;

   IncSize := True;

   // skip comments
   if (Op^.isComment) Then
    Continue;

   // if label
   if (Op^.isLabel) Then
   Begin
    BCLabel.Name       := Op^.Name;
    BCLabel.Position   := OpcodesLength;
    BCLabel.isPublic   := Op^.isPublic;
    BCLabel.isFunction := Op^.isFunction;

    if (BCLabel.isFunction) Then
     BCLabel.FunctionSymbol := TSymbol(Op^.FunctionSymbol) Else
     BCLabel.FunctionSymbol := nil;

    LabelList.Add(BCLabel);
   End Else

   // if opcode
   Begin
    if (Op^.Opcode in [o_bool, o_char, o_int, o_float, o_string]) Then
    Begin
     Case Op^.Opcode of
      o_bool  : OpcodeSize := 1;
      o_char  : OpcodeSize := 1;
      o_int   : OpcodeSize := 8;
      o_float : OpcodeSize := 8;
      o_string: OpcodeSize := 2+Length(Op^.Args[0].Value);
     End;

     Continue;
    End;

    // opcode ID itself is always one byte long
    Inc(OpcodeSize, 1);

    // special jump/call opcodes
    if (Op^.Opcode in [o_jmp, o_tjmp, o_fjmp, o_call]) and (AllocateGlobalVars) Then
    Begin
     Inc(OpcodeSize, 8 + 1);
     IncSize := False;
    End;

    // iterate each argument
    For ArgID := 0 To High(Op^.Args) Do
    Begin
     // fetch pointer
     Arg := @Op^.Args[ArgID];

     // parameter type is always one byte long
     if (IncSize) Then
      Inc(OpcodeSize, 1);

     // if not string
     if (Arg^.Typ <> ptString) Then
     Begin
      Value := VarToStr(Arg^.Value);

      // label reference
      if (Length(Value) > 2) and (Value[1] in [':', '@']) Then
      Begin
       TmpValue := Copy(Value, 2, Length(Value));

       if (Copy(TmpValue, 1, 10) = '$function.') Then
       Begin
        Delete(TmpValue, 1, 10);

        Pnt      := Pointer(StrToInt(TmpValue));
        TmpValue := TFunction(Pnt).LabelName;

        if (Length(TmpValue) = 0) Then
         raise ELLCompilerException.CreateFmt('Couldn''t fetch function''s label name; function: %s', [TFunction(Pnt).RefSymbol.getFullName('::')]);

        Arg^.Value := Value[1] + TmpValue; // copy ':' or '@' and append actual function''s label name
       End;
      End;

      // label relative address
      if (Copy(Value, 1, 1) = ':') Then
      Begin
       Arg^.Typ := ptInt;
      End;

      // label absolute address
      if (Copy(Value, 1, 1) = '@') Then
      Begin
       Arg^.Typ   := ptLabelAbsoluteReference;
       Arg^.Value := CreateReference(Copy(Value, 2, Length(Value))); // remove the beginning `@` char
      End;

      // char
      if (Copy(Value, 1, 1) = '#') Then
      Begin
       Arg^.Typ := ptChar;

       Delete(Value, 1, 1);

       if (TryStrToInt(Value, CharCode)) Then
        Arg^.Value := CharCode Else
        raise ELLCompilerException.CreateFmt('Invalid char code: %s', [Value]);
      End;

      // register
      if (isRegisterName(Value)) Then
      Begin
       Arg^.Typ   := TPrimaryType(uint8(getRegister(Value).Typ) - uint8(ptBool)); // get register type
       Arg^.Value := getRegister(Value).ID;
      End;

      // boolean truth
      if (Value = 'true') or (Value = 'True') Then
      Begin
       Arg^.Typ   := ptBool;
       Arg^.Value := 1;
      End;

      // boolean false
      if (Value = 'false') or (Value = 'False') Then
      Begin
       Arg^.Typ   := ptBool;
       Arg^.Value := 0;
      End;
     End;

     if (IncSize) Then
     Begin
      Case Arg^.Typ of
       ptBoolReg..ptReferenceReg: Inc(OpcodeSize, 1);
       ptBool, ptChar           : Inc(OpcodeSize, 1);
       ptInt                    : Inc(OpcodeSize, 8);
       ptFloat                  : Inc(OpcodeSize, 8);
       ptString                 : Inc(OpcodeSize, 2+Length(VarToStr(Arg^.Value))); // string length (uint16) + string data
       ptStackval               : Inc(OpcodeSize, 1);
       ptConstantMemRef         : Inc(OpcodeSize, 8);
       ptSymbolMemRef           : Inc(OpcodeSize, 8);
       ptLabelAbsoluteReference : Inc(OpcodeSize, 8);

       else
        raise ELLCompilerException.CreateFmt('Invalid opcode argument type: #%d', [ord(Arg^.Typ)]);
      End;
     End;
    End;
   End;
  End;
 End;
End;

(* TCompiler.Parse *)
Procedure TCompiler.Parse(const ResolveReferences: Boolean);
Var OpcodeBegin: LongWord;
    Opcode     : PMOpcode;
    Tmp        : int32;
    Arg        : int8;

    Str: String;
    Int: int32;
Begin
 With BytecodeStream do
 Begin
  For Opcode in Compiler.OpcodeList Do
  Begin
   With Opcode^ do
   Begin
    OpcodeBegin := BytecodeStream.Position;

    // skip labels and comments
    if (isLabel) or (isComment) Then
     Continue;

    // special opcodes
    if (Opcode in [o_bool, o_char, o_int, o_float, o_string]) Then
    Begin
     // @TODO: it has to be done some another way - what about architectures which are big-endian? (from the VM point of view)

     Case Opcode of
      o_bool  : write_uint8(ord(Boolean(Args[0].Value)));
      o_char  : write_uint8(Args[0].Value);
      o_int   : write_int64(Args[0].Value);
      o_float : write_float(Args[0].Value);
      o_string: write_string(Args[0].Value);
     End;

     Continue;
    End;

    // write opcode type
    write_uint8(ord(Opcode));

    // write each argument
    For Arg := Low(Args) To High(Args) Do
    Begin
     With Args[Arg] do
     Begin
      // label absolute reference
      if (Typ = ptLabelAbsoluteReference) Then
      Begin
       if (ResolveReferences) Then
       Begin
        if ((VarType(Value) and VarTypeMask) in [varInteger, varLongword, varInt64]) Then
        Begin
         Value := PChar(ReferenceStream.getData + uint32(Value));
        End;

        // find the reference
        Tmp := getLabelID(Value);

        // not found
        if (Tmp = -1) Then
        Begin
         self.Compiler.CompileError(Token, eLinker_UnknownReference, [VarToStr(Value)]);
         Exit;
        End;

        // found
        Typ   := ptInt;
        Value := LabelList[Tmp].Position;
       End Else
       Begin
        if ((VarType(Value) and VarTypeMask) = varString) Then
         Value := CreateReference(Value);
       End;
      End;

      // symbol memory reference
      if (Typ = ptSymbolMemRef) Then
      Begin
       if (ResolveReferences) Then
       Begin
        Typ   := ptConstantMemRef;
        Value := AllocateGlobalVar(Value);
       End Else
       Begin
        Value := CreateReference(Value);
       End;
      End;

      // fetch label address
      if (Typ = ptInt) and (Copy(Value, 1, 1) = ':') Then
      Begin
       Str := VarToStr(Value);
       Delete(Str, 1, 1); // remove `:`

       Int := getLabelID(Str);

       With HLCompiler.TCompiler(Compiler) do
       Begin
        // label not found
        if (Int = -1) Then
        Begin
         if (Token = nil) Then
          HLCompiler.TCompiler(Compiler).CompileError(eBytecode_LabelNotFound, [Str]) Else
          HLCompiler.TCompiler(Compiler).CompileError(Token, eBytecode_LabelNotFound, [Str]);

         Value := 0;
        End Else
        Begin
         // label found
         Value := int64(LabelList[Int].Position)-OpcodeBegin; // jumps have to be relative against the beginning of the current opcode
        End;
       End;
      End;

      // special jump/call opcodes
      if (Opcode in [o_jmp, o_tjmp, o_fjmp, o_call]) and (ResolveReferences) Then
      Begin
       write_int64(Value);
       write_uint8(0); // value doesn't matter, it it here just to fill the opcode size (9 bytes)
       Continue;
      End;

      Try
       // write parameter type
       write_uint8(ord(Typ));

       // write parameter value
       Case Typ of
        ptBoolReg..ptReferenceReg: write_uint8(Value);
        ptBool, ptChar           : write_uint8(Value);
        ptInt                    : write_int64(Value);
        ptFloat                  : write_float(Value);
        ptString                 : write_string(Value);
        ptStackval               : write_int8(Value);
        ptConstantMemRef         : write_int64(Value);
        ptSymbolMemRef           : write_int64(Value);

        ptLabelAbsoluteReference:
        Begin
         if (ResolveReferences) Then
          raise ELLCompilerException.Create('ptLabelAbsoluteReference and ResolveReferences');

         write_int64(Value);
        End;

        else
         raise ELLCompilerException.CreateFmt('Cannot compile opcode; invalid argument type: #%d', [ord(Typ)]);
       End;
      Except
       On E: ELLCompilerException Do
        raise;

       On E: Exception Do
        raise ELLCompilerException.CreateFmt('Cannot compile opcode; invalid parameter value (kind #%d): %s', [ord(Typ), VarToStr(Value)]);
      End;
     End;
    End;
   End;
  End;
 End;
End;

(* TCompiler.Create *)
Constructor TCompiler.Create;
Begin
 LabelList       := TBCLabelList.Create;
 AllocSymbolList := TBCAllocatedSymbolList.Create;
End;

(* TCompiler.Destroy *)
Destructor TCompiler.Destroy;
Begin
 LabelList.Free;
 AllocSymbolList.Free;

 inherited Destroy;
End;

(* TCompiler.Compile *)
Procedure TCompiler.Compile(const fCompiler: HLCompiler.TCompiler; const SaveAs_SSM: Boolean);
Var Output: String;
    Zip   : TZipper;

  { AddFile }
  Procedure AddFile(const Stream: TStream; const FileName: String);
  Begin
   Stream.Position := 0;
   Zip.Entries.AddFileEntry(Stream, FileName);
  End;

Var Debug: TBCDebugWriter;
Begin
 Log('-> running low-level compiler');

 Compiler := fCompiler;
 Output   := Compiler.OutputFile;

 // set variables
 DebugDataStream := nil;

 // create classes
 HeaderStream    := TStream.Create;
 BytecodeStream  := TStream.Create;
 ReferenceStream := TStream.Create;

 Zip := TZipper.Create;

 // preparse bytecode
 Preparse(not SaveAs_SSM);

 Try
  // parse bytecode
  Parse(not SaveAs_SSM);

  // save header
  With HeaderStream do
  Begin
   write_uint32($0DEFACED);

   write_uint8(BytecodeVersion.Major);
   write_uint8(BytecodeVersion.Minor);

   if (SaveAs_SSM) Then
    write_uint8(0) { not runnable } Else
    write_uint8(1) { runnable };
  End;

  // log some data
  Log('Header size: %d bytes', [HeaderStream.Size]);
  Log('References data size: %d bytes', [ReferenceStream.Size]);
  Log('Bytecode size: %d bytes', [BytecodeStream.Size]);

  // save as a library?
  if (SaveAs_SSM) Then
  Begin
   With TSSMWriter.Create(Compiler.OutputFile, fCompiler, self) do
   Begin
    Save;
    Free;
   End;

   Exit;
  End;

  // generate debug data
  if (not CmdLine.getBoolSwitch(opt__strip_debug)) Then
  Begin
   Debug := TBCDebugWriter.Create(fCompiler, self);

   Try
    DebugDataStream := Debug.Generate(True);
   Finally
    Debug.Free;
   End;

   Log('Debug data size: %d bytes', [DebugDataStream.Size]);
  End;

  // make archive
  AddFile(HeaderStream, '.header');
  AddFile(BytecodeStream, '.bytecode');

  if (not CmdLine.getBoolSwitch(opt__strip_debug)) Then
   AddFile(DebugDataStream, '.debug');

  // save it
  Zip.FileName := Output;
  Zip.ZipAllFiles;
 Finally
  Zip.Free;
  HeaderStream.Free;
  DebugDataStream.Free;
  BytecodeStream.Free;
 End;
End;

End.
