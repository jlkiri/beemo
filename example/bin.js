function binarySearch(arr, num) {
  let lo = 0;
  let hi = arr.length - 1;
  while (lo <= hi) {
    let mid = lo + Math.floor((hi - lo) / 2);
    if (arr[mid] == num) {
      return mid;
    }
    if (arr[mid] > num) {
      hi = mid;
    }
    if (arr[mid] < num) {
      lo = mid;
    }
  }
  return -1;
}

let arr = [];
let counter = 0;

console.time();
while (counter < 100000) {
  arr.push(counter);
  counter += 1;
}
console.timeEnd();

console.time();
console.log(binarySearch(arr, 32410));
console.timeEnd();
