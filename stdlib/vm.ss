@visibility("public")

namespace std
{
 function<void> vm_exit(int) [library="vm.ssm"];
 function<int> vm_loadlibrary(string) [library="vm.ssm"];
}
