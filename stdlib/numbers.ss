@visibility("public")

}
namespace std
{
 function<string> intsys(int, int) in "numbers.ssm";
 function<int> sysint(string, int) in "numbers.ssm";
 function<int> intlen(int) in "numbers.ssm";
 function<string> intstr(int) in "numbers.ssm";
 function<int> strint(string) in "numbers.ssm";
 function<string> intbin(int) in "numbers.ssm";
 function<int> binint(string) in "numbers.ssm";
 function<string> inthex(int) in "numbers.ssm";
 function<int> hexint(string) in "numbers.ssm";
 function<string> fltstr(float) in "numbers.ssm";
 function<float> strflt(string) in "numbers.ssm";
}
