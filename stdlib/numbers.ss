@visibility("public")

namespace std
{
 function<string> intsys(int, int) [library="numbers.ssm"];
 function<int> sysint(string, int) [library="numbers.ssm"];
 function<int> intlen(int) [library="numbers.ssm"];
 function<string> intstr(int) [library="numbers.ssm"];
 function<int> strint(string) [library="numbers.ssm"];
 function<int> strintdef(string, int) [library="numbers.ssm"];
 function<string> intbin(int) [library="numbers.ssm"];
 function<int> binint(string) [library="numbers.ssm"];
 function<string> inthex(int) [library="numbers.ssm"];
 function<int> hexint(string) [library="numbers.ssm"];
 function<string> fltstr(float) [library="numbers.ssm"];
 function<float> strflt(string) [library="numbers.ssm"];
 function<float> strfltdef(string, float) [library="numbers.ssm"];
}
