(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.

 LibInfo compiler feature.
*)
Unit LibInfo;

 Interface

 Procedure RunLibInfo(const InputFile: String);

 Implementation
Uses SysUtils, TypInfo, CommandLine, SSMParser, symdef;

(* RunLibInfo *)
Procedure RunLibInfo(const InputFile: String);
Var Reader: TSSMReader;

    NamespaceList: TNamespaceList;
    Namespace    : TNamespace;
    Symbol       : TSymbol;

    mFunction: TFunction;
    mVariable: TVariable;

    I: uint32;
Begin
 Reader := TSSMReader.Create(nil, InputFile);

 Try
  Try
   if (not Reader.Load) Then
    raise Exception.Create('Couldn''t load library.');

   NamespaceList := Reader.getNamespaceList;

   Writeln('libinfo begin');

   For Namespace in NamespaceList Do
   Begin
    Writeln('namespace ', Namespace.RefSymbol.Name);

    For Symbol in Namespace.getSymbolList Do
    Begin
     Case Symbol.Typ of
      // function
      stFunction:
      Begin
       mFunction := Symbol.mFunction;

       Write('function; ',
             'name=', Symbol.Name, '; ',
             'return=', mFunction.Return.RefSymbol.getFullName('::'), '; ',
             'parameter_count=', Length(mFunction.ParamList), '; ',
             'parameter_types=');

       if (Length(mFunction.ParamList) > 0) Then
       Begin
        For I := 0 To High(mFunction.ParamList) Do
        Begin
         Write(mFunction.ParamList[I].Typ.RefSymbol.getFullName('::'));

         if (I < uint32(High(mFunction.ParamList))) Then
          Write(', ');
        End;
       End;

       Writeln(';');
      End;

      // variable
      stVariable:
      Begin
       mVariable := Symbol.mVariable;

       Writeln('variable; ',
               'name=', Symbol.Name, '; ',
               'type=', mVariable.Typ.RefSymbol.getFullName('::'), ';');
      End;

      // constant
      stConstant:
      Begin
       Writeln('constant; ',
               'name=', Symbol.Name, ';');
      End;

      // type
      stType:
      Begin
       Writeln('type; ',
               'name=', Symbol.Name, ';');
      End;
     End;
    End;
   End;

   Writeln('libinfo end');
  Except
   On E: Exception Do
   Begin
    Writeln('libinfo exception');
    Writeln(E.Message);
   End;
  End;
 Finally
  Reader.Free;
 End;
End;

End.
