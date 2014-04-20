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
}