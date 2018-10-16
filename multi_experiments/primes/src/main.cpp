#include <cmath>
#include <cassert> 
#include <iostream> 
#include <sstream>
#include <vector>
#include <thread>
#include <mutex>

// checks if num is prime
bool isPrime(int num)  {
	if(num <= 1)
		return false;

	for(int div = 2; div < num / 2 + 1; ++div)
		if(num % div == 0)
			return false;
	return true;
}

// finds the prime after the num given
int nextPrime(int num) {
	if(num <= 1)
		return 2;

	if(num % 2 == 0) ++num;
	else num += 2;
	for(; !isPrime(num); num += 2);
	return num;
}

// find the prime before the num given
int prevPrime(int num) {
	if(num <= 1)
		return 0;

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
size_t countPrimesInRange(int lowerLimit, int upperLimit) {
	// calculate primes within bounds
	size_t primesFound = 0;
	for(int prime = nextPrime(lowerLimit); prime <= upperLimit; ++primesFound)
		prime = nextPrime(prime);
	
	return primesFound;
}

// find nth prime using threads
int nthPrimeParallel(size_t nth, size_t threadsNum = 2) {
	// make initial approximation of nth prime
	int approx = nth * std::log(nth);

	// calculate calculator range and adjust approximation
	approx -= approx % threadsNum;
	size_t range = approx / threadsNum;

	// holds all primes found by prime counting threads
	size_t primesFound = 0;

	// create calculator threads
	std::vector<std::thread> calcThreads;
	for(size_t j = 0; j < threadsNum ; ++j)
		calcThreads.emplace_back([&primesFound, j, range] {
			primesFound += countPrimesInRange(j * range, (j + 1) * range);
		}
	);

	// wait for calculators to be done
	for(auto& thread : calcThreads)
		thread.join();

	// use info collected by controller in combination with approx to quickly calculate nth prime
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
int main(int argc, char* argv[]) {
	//DEBUG std::cout << "primes:\t\tresult\t\tapprox\t\tfound\t\tnth:" << std::endl;
	
	size_t nth, threadsNum;
	std::stringstream ss;
	ss << argv[1] << " " << argv[2];
	ss >> nth;
	ss >> threadsNum;
	std::cout << nthPrimeParallel(nth, threadsNum) << std::endl;

	return 0;
}
