@visibility("public")

namespace std
{
 function<void> print(any) [library="stdio.ssm"];
 function<void> println(any) [library="stdio.ssm"];
 function<void> newline() [library="stdio.ssm"];
 function<bool> key_pressed() [library="stdio.ssm"];
 function<char> get_char() [library="stdio.ssm"];
 function<char> read_char() [library="stdio.ssm"];
 function<char> read_char_t(string) [library="stdio.ssm"];
 function<string> read_string() [library="stdio.ssm"];
 function<string> read_string_t(string) [library="stdio.ssm"];
 function<int> read_int() [library="stdio.ssm"];
 function<int> read_int_t(string) [library="stdio.ssm"];
 function<float> read_float() [library="stdio.ssm"];
 function<float> read_float_t(string) [library="stdio.ssm"];
 function<string> read_until(char) [library="stdio.ssm"];
 function<string> read_until_t(string, char) [library="stdio.ssm"];
 function<void> wait_for(char) [library="stdio.ssm"];
 function<void> wait_for_t(string, char) [library="stdio.ssm"];
 function<void> set_size_ex(int, int, int, int) [library="stdio.ssm"];
 function<void> set_size(int, int) [library="stdio.ssm"];
 function<void> set_buffered(bool) [library="stdio.ssm"];
 function<void> flush() [library="stdio.ssm"];
 function<void> clear() [library="stdio.ssm"];
 function<void> show_cursor() [library="stdio.ssm"];
 function<void> hide_cursor() [library="stdio.ssm"];
}
