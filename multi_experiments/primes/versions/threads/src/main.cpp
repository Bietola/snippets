#include <cmath>
#include <cassert> 
#include <iostream>
#include <vector>
#include <thread>
#include <mutex>

// checks if num is prime
bool isPrime(int num)  {
	if(num == 0)
		return false;

	for(int div = 2; div < num / 2 + 1; ++div)
		if(num % div == 0)
			return false;
	return true;
}

// finds the prime after the num given
int nextPrime(int num) {
	if(num < 3)
		return num + 1;
	if(num % 2 == 0) ++num;
	else num += 2;
	for(; !isPrime(num); num += 2);
	return num;
}

// find the prime before the num given
int prevPrime(int num) {
	if(num == 0)
		return 0;
	if(num < 4)
		return num - 1;

	if(num % 2 == 0) --num;
	else num -= 2;
	for(; !isPrime(num); num -= 2);
	return num;
}

// finds the nth prime number
int nthPrime(size_t n) {
	int prime;
	for(prime = 0; n > 0; --n, prime = nextPrime(prime));
	return prime;
}

// finds the number of primes in range
size_t allPrimesFound = 0;
size_t countPrimesInRange(int lowerLimit, int upperLimit) {
	// data race prevention variables
	static std::mutex updateMutex;
	
	// calculate primes within bounds
	size_t primesFound = 0;
	for(int prime = nextPrime(lowerLimit); prime <= upperLimit; ++primesFound)
		prime = nextPrime(prime);

	// update counter
	updateMutex.lock();
	allPrimesFound += primesFound;
	updateMutex.unlock();
}

// find nth prime using threads
int nthPrimeParallel(size_t nth, size_t threadsNum = 2) {
	// make initial approximation of nth prime
	int approx = nth * std::log(nth);

	// calculate calculator range and adjust approximation
	approx -= approx % threadsNum;
	size_t range = approx / threadsNum;

	// create calculator threads
	std::vector<std::thread> calcThreads;
	for(size_t j = 0; j < threadsNum ; ++j)
		calcThreads.emplace_back(countPrimesInRange, j * range, (j + 1) * range);

	// wait for calculators to be done
	//  NB: meanwhile, the controller is being updated 
	for(auto& thread : calcThreads)
		thread.join();

	// use info collected by controller in combination with approx to quickly calculate nth prime
	size_t primesFound = allPrimesFound;
	int result = approx;
	if(primesFound < nth) {
		for(size_t j = 0; j < nth - primesFound; ++j)
			result = nextPrime(result);
	}
	else if(primesFound > nth) {
		for(size_t j = 0; j < primesFound - nth; ++j)
			result = prevPrime(result);
	}
	else if(!isPrime(result)){
		result = prevPrime(approx);
	}

	/* DEBUG
	int realResult = nthPrime(nth);
	std::cout << realResult << "\t\t" << result << "\t\t" << approx 
		<< "\t\t" << primesFound << "\t\t" << nth << std::endl;
	*/
	
	// done!
	return result;
}

// MAIN
int main() {
	//DEBUG std::cout << "primes:\t\tresult\t\tapprox\t\tfound\t\tnth:" << std::endl;
	
	std::cout << nthPrimeParallel(100, 4) << std::endl;

	return 0;
}
