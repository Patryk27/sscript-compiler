@visibility("public")

namespace std
{
 function<char> chupper(char) [library="string.ssm"];
 function<char> chlower(char) [library="string.ssm"];
 function<bool> isdigit(char) [library="string.ssm"];
 function<bool> isxdigit(char) [library="string.ssm"];
 function<bool> isalpha(char) [library="string.ssm"];
 function<bool> isalnum(char) [library="string.ssm"];
 function<bool> isspace(char) [library="string.ssm"];
 function<int> strlen(string) [library="string.ssm"];
 function<int> strcount(string, char) [library="string.ssm"];
 function<string> strreverse(string) [library="string.ssm"];
 function<string> strcopy(string, int, int) [library="string.ssm"];
 function<string> strdelete(string, int, int) [library="string.ssm"];
 function<string> strdelete_begin(string, int) [library="string.ssm"];
 function<string> strdelete_end(string, int) [library="string.ssm"];
 function<int> strpos_ex(string, string, int) [library="string.ssm"];
 function<int> strpos(string, string) [library="string.ssm"];
 function<bool> strfind(string, string) [library="string.ssm"];
 function<string> strtrim_l(string) [library="string.ssm"];
 function<string> strtrim_r(string) [library="string.ssm"];
 function<string> strtrim(string) [library="string.ssm"];
 function<string> strremove_begin(string, char) [library="string.ssm"];
 function<string> strremove_end(string, char) [library="string.ssm"];
 function<string> strupper(string) [library="string.ssm"];
 function<string> strlower(string) [library="string.ssm"];
 function<string> strdup(string, int) [library="string.ssm"];
 function<bool> strstarts(string, string) [library="string.ssm"];
 function<bool> strends(string, string) [library="string.ssm"];
 function<string> strinsert(string, string, int) [library="string.ssm"];
 function<string> strreplace_ex(string, string, string, bool) [library="string.ssm"];
 function<string> strreplace(string, string, string) [library="string.ssm"];
 function<string> strreplace_once(string, string, string) [library="string.ssm"];
 function<string> strleft(string, int) [library="string.ssm"];
 function<string> strright(string, int) [library="string.ssm"];
 function<bool> strcmp(string, string) [library="string.ssm"];
 function<bool> strcmpl(string, string) [library="string.ssm"];
 function<string> boolstr(bool) [library="string.ssm"];
 function<string> boolstrb(bool, string, string) [library="string.ssm"];
 function<string[]> strexplode(string, char) [library="string.ssm"];
 function<string> strimplode(string[], string) [library="string.ssm"];
 function<string> streach(string, function<char>(char)) [library="string.ssm"];
 function<string> streach_index(string, function<char>(char, int)) [library="string.ssm"];
 function<string> streach_index_string(string, function<char>(char, int, string)) [library="string.ssm"];
 function<char[]> stringarray(string) [library="string.ssm"];
 function<string> arraystring(char[]) [library="string.ssm"];
}
