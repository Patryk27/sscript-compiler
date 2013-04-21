/*
 SScript Standard Library
 stdio.ss
*/

@("numbers.ss")
@visibility("public")

namespace std
{

/* ===== writing text on screen ===== */

function<void> print(any text) naked
{
 :CODE sub(stp, 1) // stack[stp] is caller-IP (instruction pointer)

 /*
  Now, on `stack[stp]` we have our `text` parameter (wich have unknown - for us - type); but its type doesn't actually matter.
  We use the `naked` directive to force compiler to not create the stack frame, so our function's code will be only what we'll write here.
  ..and here we have only an 'icall' opcode.
  This icall-call gets an argument from the stack and puts it directly onto the screen, so it's a perfect solution, because we have
  nothing to worry about, as the virtual machine will handle it ;)
 */
 :CODE
 {
  icall("output.print")
  add(stp, 2)
 }
}

function<void> println(any text) naked
{
 /*
  Almost the same as in 'println'; here we just need to do 2 `icall`s putting newline and caret return char.
 */
 :CODE
 {
  sub(stp, 1)

  icall("output.print")

  push(#10) // char(10) = newline char
  icall("output.print")

  push(#13) // char(13) = caret return
  icall("output.print")

  add(stp, 2)
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

/* ===== reading text from user ===== */

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

// read_char
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

// read_string
function<string> read_string() naked
{
 :CODE
 {
  icall("input.read")
  pop(es1)
 }
}

function<string> read_string_t(string text)
{
 print(text);
 return read_string();
}

// read_int
function<int> read_int()
{
 return strint(read_string());
}

function<int> read_int_t(string text)
{
 print(text);
 return read_int();
}

// read_float
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

/* ===== console settings ===== */

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
  sub(stp, 1)
  icall("output.set_buffered")
  add(stp, 2)
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
 :CODE icall("output.cursor.show")
}

function<void> hide_cursor() naked
{
 :CODE icall("output.cursor.hide")
}

}
