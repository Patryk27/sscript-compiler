@visibility("public")

namespace std
{
 function<char> chupper(char) in "string.ssm";
 function<char> chlower(char) in "string.ssm";
 function<bool> isdigit(char) in "string.ssm";
 function<bool> isxdigit(char) in "string.ssm";
 function<bool> isalpha(char) in "string.ssm";
 function<bool> isalnum(char) in "string.ssm";
 function<bool> isspace(char) in "string.ssm";
 function<int> strlen(string) in "string.ssm";
 function<string> strreverse(string) in "string.ssm";
 function<string> strcopy(string, int, int) in "string.ssm";
 function<string> strdelete(string, int, int) in "string.ssm";
 function<string> strdelete_begin(string, int) in "string.ssm";
 function<string> strdelete_end(string, int) in "string.ssm";
 function<int> strpos_ex(string, string, int) in "string.ssm";
 function<int> strpos(string, string) in "string.ssm";
 function<bool> strposb(string, string) in "string.ssm";
 function<string> strtrim_l(string) in "string.ssm";
 function<string> strtrim_r(string) in "string.ssm";
 function<string> strtrim(string) in "string.ssm";
 function<string> strremove_begin(string, char) in "string.ssm";
 function<string> strremove_end(string, char) in "string.ssm";
 function<string> strupper(string) in "string.ssm";
 function<string> strlower(string) in "string.ssm";
 function<string> strdup(string, int) in "string.ssm";
 function<string> strrot(string, int) in "string.ssm";
 function<bool> strstarts(string, string) in "string.ssm";
 function<bool> strends(string, string) in "string.ssm";
 function<string> strinsert(string, string, int) in "string.ssm";
 function<string> strreplace_ex(string, string, string, bool) in "string.ssm";
 function<string> strreplace(string, string, string) in "string.ssm";
 function<string> strreplace_once(string, string, string) in "string.ssm";
 function<string> strleft(string, int) in "string.ssm";
 function<string> strright(string, int) in "string.ssm";
 function<bool> strcmp(string, string) in "string.ssm";
 function<bool> strcmpl(string, string) in "string.ssm";
 function<string> boolstr(bool) in "string.ssm";
 function<string> boolstrb(bool, string, string) in "string.ssm";
 function<string[]> strexplode(string, char) in "string.ssm";
 function<string> strimplode(string[], char) in "string.ssm";
}
