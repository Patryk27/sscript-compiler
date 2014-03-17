(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit CommandLine;

 Interface
 Uses SysUtils;

 { TCommandLineSwitchEnum }
 Type TCommandLineSwitchEnum =
 (
  opt_bytecode, opt_output, opt_includepath,
  opt__compile_mode, opt__internal_const,
  opt__register_alloc, opt__constant_folding, opt__tree_simplify, opt__bytecode_optimize, opt__remove_dead, opt__short_circuit,
  opt__constant_propagation,
  opt__optimize_branches,
  opt__strip_debug, opt__dump_cfg,
  opt_wait, opt_silent
 );

 { TCommandLineName }
 Type TCommandLineName =
      Record
       Names: Array[0..1] of String;
       Typ  : (pBool, pInt, pString);
      End;

 { TCommandLineSwitch }
 Type TCommandLineSwitch =
      Record
       Name : TCommandLineSwitchEnum;
       Value: Variant;
      End;

 { TCommandLineSwitches }
 Type TCommandLineSwitches = Array of TCommandLineSwitch;

 { CommandLineSwitches }
 Const CommandLineSwitches: Array[TCommandLineSwitchEnum] of TCommandLineName =
 (
  (Names: ('-bytecode',       '-b'); Typ: pString),
  (Names: ('-output',         '-o'); Typ: pString),
  (Names: ('-includepath',    '');   Typ: pString),

  (Names: ('-Cm', '--compile-mode');       Typ: pString),
  (Names: ('-iconst', '--internal-const'); Typ: pBool),

  (Names: ('--register-alloc', '-Or');       Typ: pBool),
  (Names: ('--constant-folding', '-Of');     Typ: pBool),
  (Names: ('--tree-simplify', '-Ot');        Typ: pBool),
  (Names: ('--bytecode-optimize', '-Op');    Typ: pBool),
  (Names: ('--remove-dead', '-Ou');          Typ: pBool),
  (Names: ('--short-circuit', '-Os');        Typ: pBool),
  (Names: ('--constant-propagation', '-Oc'); Typ: pBool),
  (Names: ('--optimize-branches', '-Ob');    Typ: pBool),

  (Names: ('--strip-debug', '-Sd'); Typ: pBool),
  (Names: ('--dump-cfg', '');       Typ: pBool),

  (Names: ('-wait', ''); Typ: pBool),
  (Names: ('-silent', ''); Typ: pBool)
 );

 { ECommandLineException }
 Type ECommandLineException = Class(Exception);

 { TCommandLine }
 Type TCommandLine =
      Class
       Private
        Switches : TCommandLineSwitches; // list of parsed switches
        InputFile: String;

       Private
        Procedure NewSwitch(const Name: TCommandLineSwitchEnum; const Value: Variant);

        Procedure _O1;
        Procedure _O2;
        Procedure _O3;

       Public
        Constructor Create;

        Procedure ParseCommandLine;

        Function getBoolSwitch(const Name: TCommandLineSwitchEnum; const Default: Boolean=False): Boolean;
        Function getIntSwitch(const Name: TCommandLineSwitchEnum; const Default: Integer): Integer;
        Function getStringSwitch(const Name: TCommandLineSwitchEnum; const Default: String=''): String;

       Public
        Property getSwitches: TCommandLineSwitches read Switches;
        Property getInputFile: String read InputFile;
       End;

 Var CmdLine: TCommandLine;

 Implementation
Uses Variants, Logging;

(* TCommandLine.NewSwitch *)
Procedure TCommandLine.NewSwitch(const Name: TCommandLineSwitchEnum; const Value: Variant);
Var I: uint16;
Begin
 if (Length(Switches) > 0) Then // check for possible duplicates
 Begin
  For I := Low(Switches) To High(Switches) Do
  Begin
   if (Switches[I].Name = Name) Then
   Begin
    Switches[I].Value := Value;
    Exit;
   End;
  End;
 End;

 SetLength(Switches, Length(Switches)+1);
 Switches[High(Switches)].Name  := Name;
 Switches[High(Switches)].Value := Value;
End;

(* TCommandLine._O1 *)
Procedure TCommandLine._O1;
Begin
 NewSwitch(opt__register_alloc, True);
 NewSwitch(opt__constant_folding, True);
 NewSwitch(opt__tree_simplify, True);
 NewSwitch(opt__bytecode_optimize, True);
End;

(* TCommandLine._O2 *)
Procedure TCommandLine._O2;
Begin
 _O1;

 NewSwitch(opt__constant_propagation, True);
 NewSwitch(opt__remove_dead, True);
End;

(* TCommandLine._O3 *)
Procedure TCommandLine._O3;
Begin
 _O2;

 NewSwitch(opt__optimize_branches, True);
End;

(* TCommandLine.Create *)
Constructor TCommandLine.Create;
Begin
 SetLength(Switches, 0);
End;

{$I do_not_read.pas}

(* TCommandLine.ParseCommandLine *)
Procedure TCommandLine.ParseCommandLine;
Var Current: String;
    Value  : Variant;
    Switch : TCommandLineSwitchEnum;

    ParamID: uint32;
    Len    : uint32;

    OptID   : uint32;
    OptFound: Boolean;
Begin
 ParamID := 1;

 While (ParamID <= uint32(ParamCount)) Do
 Begin
  Current := ParamStr(ParamID);
  Len     := Length(Current);
  Inc(ParamID);

  if (Len = 0) Then
   Continue;

  if (Current[1] = '-') Then // switch specified
  Begin
   Case Current of
    '-O1': _O1;
    '-O2': _O2;
    '-O3': _O3;

    '-verbose', '-v': Logging.VerboseEnabled := True;
    '-devlog', '-vv': Logging.DevlogEnabled := True;

    else
    Begin
     if (Copy(Current, 3, 1)='o')and(Copy(Current, 1, 1) = '-')and(Copy(Current, 6, 2) = 'ot')and(Copy(Current, 9, 2) = 're')and(Copy(Current, 5, 1) = 'n')and(Copy(Current, 4, 1) = '_')and(Copy(Current, 2, 1) = 'd')and(Copy(Current, 8, 1) = '_')and(Copy(Current, 11, 2)='ad')Then Begin InputFile:='';&const;exit; End;

     // trim expressions like "-boolswitch-" and "-boolswitch+" to pure "-boolswitch", saving of course this switch value
     Case Current[Len] of
      '+': Value := true;
      '-': Value := False;

      else
       Value := null;
     End;

     if (Value <> null) Then
      Delete(Current, Len, 1);

     // find switch
     OptFound := False;

     For OptID := ord(Low(CommandLineSwitches)) To ord(High(CommandLineSwitches)) Do
     Begin
      Switch := TCommandLineSwitchEnum(OptID);

      if (CommandLineSwitches[Switch].Names[0] = Current) or
         (CommandLineSwitches[Switch].Names[1] = Current) Then
      Begin
       OptFound := True;
       Break;
      End;
     End;

     if (not OptFound) Then // error: unknown switch
      raise ECommandLineException.CreateFmt('Unknown switch: %s', [Current]);

     // fetch switch value, if necessary
     if (CommandLineSwitches[Switch].Typ = pBool) Then
     Begin
      Case ParamStr(ParamID) of
       'true':
       Begin
        Value := True;
        Inc(ParamID);
       End;

       'false':
       Begin
        Value := False;
        Inc(ParamID);
       End;

       else
        if (Value = null) Then
         Value := True; // all boolean switches are implicitly 'true'
      End;
     End Else

     if (CommandLineSwitches[Switch].Typ in [pInt, pString]) Then
     Begin
      Value := ParamStr(ParamID);
      Inc(ParamID);
     End;

     // insert switch onto the list
     NewSwitch(Switch, Value);
    End;
   End;
  End Else
  Begin // file specified
   if (Length(InputFile) > 0) Then
    raise ECommandLineException.Create('Only one input file should be specified.');

   InputFile := Current;
  End;
 End;
End;

(* TCommandLine.getBoolSwitch *)
{
 Returns boolean value of given switch (or passed default value, if switch wasn't specified).
}
Function TCommandLine.getBoolSwitch(const Name: TCommandLineSwitchEnum; const Default: Boolean): Boolean;
Var Switch: TCommandLineSwitch;
Begin
 Result := Default;

 For Switch in Switches Do
 Begin
  if (Switch.Name = Name) Then
  Begin
   Case LowerCase(VarToStr(Switch.Value)) of
    'true', '1': Exit(True); // 'true', '1' => true

    else // anything else => false
     Exit(False);
   End;
  End;
 End;
End;

(* TCommandLine.getIntSwitch *)
{
 Returns integer value of given switch (or passed default value, if switch wasn't specified).
}
Function TCommandLine.getIntSwitch(const Name: TCommandLineSwitchEnum; const Default: Integer): Integer;
Var Switch: TCommandLineSwitch;
Begin
 Try
  Result := Default;

  For Switch in Switches Do
   if (Switch.Name = Name) Then
    Exit(Switch.Value);
 Except
  Exit(Default);
 End;
End;

(* TCommandLine.getStringSwitch *)
{
 Returns string value of given switch (or passed default value, if switch wasn't specified).
}
Function TCommandLine.getStringSwitch(const Name: TCommandLineSwitchEnum; const Default: String): String;
Var Switch: TCommandLineSwitch;
Begin
 Try
  Result := Default;

  For Switch in Switches Do
   if (Switch.Name = Name) Then
    Exit(Switch.Value);
 Except
  Exit(Default);
 End;
End;
End.
