{ ParseMacro_Outside }
Procedure ParseMacro_Outside;

  { @visibility }
  Procedure _visibility;
  Var Str: String;
  Begin
   With Parser do
   Begin
    Str := read.Display;
    Case Str of
     'public' : Visibility := mvPublic;
     'private': Visibility := mvPrivate;
     else Visibility := mvPrivate;
    End;
   End;
  End;

Var Name: String;
Begin
 { read identifier }
 With Parser do
  if (next_t = _IDENTIFIER) Then
  Begin
   Name := read_ident;
   eat(_BRACKET1_OP);
   { parse macro }
   Case Name of
    'visibility': _visibility;
    else CompileError(eUnknownMacro, [Name]);
   End;
   eat(_BRACKET1_CL);
  End Else
   Parse_include.Parse(self);
End;
