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

console.log(binarySearch([1,2,3,4,5,6,7,8,9], 7));