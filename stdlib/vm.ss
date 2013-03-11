@visibility("public")

namespace std
{
 function<void> exit(int) in "vm.ssm";
 function<void> throwError(string) in "vm.ssm";
 function<string> getLastError() in "vm.ssm";
}
