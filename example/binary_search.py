import time

def binary_search(array, num):
	lo = 0
	hi = len(array) - 1
	while lo <= hi:
		mid = lo + (hi - lo) // 2
		if array[mid] == num:
			return mid
		if array[mid] > num:
			hi = mid
		if array[mid] < num:
			lo = mid + 1
	return -1

def main():
	counter = 0
	array = []
	while counter < 100000:
		array.append(counter)
		counter += 1
	start_time = time.perf_counter()
	print(binary_search(array, 32410))
	print("--- %s ms ---" % ((time.perf_counter() - start_time) * 1000))

main()
