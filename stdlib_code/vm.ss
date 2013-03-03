/*
 SScript Standard Library
 time.ss
*/

@visibility("public")

namespace std
{

function<void> exit(int exitcode) naked
{
 :CODE icall("vm.exit")
}

}