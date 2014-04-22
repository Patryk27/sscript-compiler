/*
 SScript Standard Library
 random.ss

 32-bit Mersenne Twister randomizer.
*/

@("time.ss")

const mt_size = 624;

var<int[]> mt;
var<int> index;

public namespace std
{
 /*
  @name: generate_table

  Generates new number table.
  Shouldn't be called explicitly (that's why it's private).
 */
 private function generate_table()
 {
  for (var<int> i=0; i<mt_size; i++)
  {
   var<int> y = ((mt[i] >> 32) & 1) + (mt[(i+1) % mt_size] & 0x7FFFFF);

   mt[i] = mt[(i+397) % mt_size] ^ (y >> 1);

   if (y&1 == 0)
   {
    mt[i] = mt[i] ^ 0x9908b0df;
   }
  }
 }

 public;

 /*
  @name: init_randomizer
  @param int seed: seed for the randomizer

  Initializes randomizer with given seed.
 */
 function init_randomizer(const int seed)
 {
  // reset index
  index = 0;

  // allocate the array
  mt = new int[mt_size];

  // fill the array
  mt[0] = seed;
  for (var<int> i=1; i<mt_size; i++)
   mt[i] = (0x6c078965 * (mt[i-1] ^ (mt[i-1] >> 30)) + i);
 }

 /*
  @name: randomize

  Initializes randomizer with seed equal to value returned from "get_milliseconds".
 */
 function randomize()
 {
  init_randomizer(get_milliseconds());
 }

 /*
  @name: random

  Returns a random 32-bit number.
 */
 function<int> random()
 {
  if (index == 0)
   generate_table();

  var<int> y = mt[index];

  y ^= y >> 11;
  y ^= (y << 7) & 0x9d2c5680;
  y ^= (y << 15) & 0xefc60000;
  y ^= (y >> 18);

  index = (index+1) % mt_size;

  return y & 0xFFFFFF;
 }

 /*
  @name: random_8

  Returns a random 8-bit number.
 */
 function<int> random_8()
 {
  return random() & 0xFF;
 }

 /*
  @name: random_16

  Returns a random 16-bit number.
 */
 function<int> random_16()
 {
  return random() & 0xFFFF;
 }

 /*
  @name: random

  Returns a random 32-bit number.
 */
 function<int> random_32()
 {
  return random();
 } 

 /*
  @name: random_64

  Returns a random 64-bit number.
 */
 function<int> random_64()
 {
  return (random() << 32) | random();
 }

 /*
  @name: random_range

  Returns a random 54-bit number from specified range.
 */
 function<int> random_range(const int low, const int high)
 {
  return random()%high + low;
 }
}
