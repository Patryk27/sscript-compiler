/*
 SScript Standard Library
 stdio.ss
*/

@("numbers.ss")

public;

namespace std
{
 function<void> print(any text) naked
 {
  :CODE
  {
   push(%text)
   icall("output.print")
  }
 }

 function<void> println(any text) naked
 {
  :CODE
  {
   push(%text)
   icall("output.print")

   push(#10) // char(10) = newline char
   icall("output.print")

   push(#13) // char(13) = caret return
   icall("output.print")
  }
 }

 function<void> newline() naked
 {
  :CODE
  {
   push(#10)
   icall("output.print")

   push(#13)
   icall("output.print")
  }
 }

 function<bool> key_pressed() naked
 {
  :CODE
  {
   icall("input.keypressed")
   pop(eb1)
  }
 }

 function<char> get_char() naked
 {
  :CODE
  {
   icall("input.getchar")
   pop(ec1)
  }
 }

 function<char> read_char() naked
 {
  :CODE
  {
   icall("input.readchar")
   pop(ec1)
  }
 }

 function<char> read_char_t(string text)
 {
  print(text);
  return read_char();
 }

 function<string> read_string() naked
 {
  :CODE
  {
   icall("input.read")
   pop(es0)
  }
 }

 function<string> read_string_t(string text)
 {
  print(text);
  return read_string();
 }

 function<int> read_int()
 {
  return strint(read_string());
 }

 function<int> read_int_t(string text)
 {
  print(text);
  return read_int();
 }

 function<float> read_float()
 {
  return strflt(read_string());
 }

 function<float> read_float_t(string text)
 {
  print(text);
  return read_float();
 }

 function<string> read_until(char terminator)
 {
  var<string> result = "";

  while (true)
  {
   var<char> ch = read_char();

   if (ch == terminator) // read until the terminator char (excluding it from the string)
    return result;
  
   result += ch;
  }
 }

 function<string> read_until_t(string text, char terminator)
 {
  print(text);
  return read_until(terminator);
 }

 function<void> wait_for(char ch)
 {
  if (ch == null) // wait for any key
  {
   while (!key_pressed()) ;
  } else // wait for specified key
  {
   while (get_char() != ch) ;
  }
 }

 function<void> wait_for_t(string text, char ch)
 {
  print(text);
  wait_for(ch);
 }

 function<void> set_size_ex(int console_width, int console_height, int crtwindow_width, int crtwindow_height) naked
 {
  :CODE
  {
   sub(stp, 1)
   icall("output.set_size")
   add(stp, 5)
  }
 }

 function<void> set_size(int width, int height)
 {
  set_size_ex(width, height, width, height);
 }

 function<void> set_buffered(bool buffered) naked
 {
  :CODE
  {
   push(%buffered)
   icall("output.set_buffered")
  }
 }

 function<void> flush() naked
 {
  :CODE icall("output.flush")
 }

 function<void> clear() naked
 {
  :CODE icall("output.clear")
 }

 function<void> show_cursor() naked
 {
  :CODE icall("output.cursor_show")
 }

 function<void> hide_cursor() naked
 {
  :CODE icall("output.cursor_hide")
 }
}