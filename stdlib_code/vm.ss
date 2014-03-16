/*
 SScript Standard Library
 vm.ss
*/

public;

namespace std
{
 function<void> vm_exit(int exitcode) naked
 {
  :CODE
  {
   push(%exitcode)
   icall("vm.exit")
  }
 }

 function<int> vm_loadlibrary(string libfile) naked
 {
  :CODE
  {
   push(%libfile)
   icall("vm.loadlibrary")
   pop(ei1)
  }
 }
}