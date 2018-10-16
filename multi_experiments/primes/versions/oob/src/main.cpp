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

// prime async calculator
class Controller;
class Calculator {
	private:
		size_t lowerLimit, upperLimit;
		std::weak_ptr<Controller> controller;

		size_t primesFound = 0;
		
	public:
		Calculator(size_t lowerLimit, size_t upperLimit, std::weak_ptr<Controller> controller);

		void operator()();

		size_t getResult() const {return primesFound;}
};

// prime controller
class Controller {
	private:
		size_t primesFound = 0;
		size_t calculatorsDoneNum = 0;

		std::mutex updateMutex;

	public:
		// update info when calculator is done
		void update(const Calculator& calculator);

		size_t getPrimesFound() const {return primesFound;}
};

// prime calculator functions
Calculator::Calculator(size_t lowerLimit, size_t upperLimit, std::weak_ptr<Controller> controller):
	lowerLimit(lowerLimit),
	upperLimit(upperLimit),
	controller(controller) {}

void Calculator::operator()() {
	// calculate primes within bounds
	for(int prime = nextPrime(lowerLimit); prime <= upperLimit; ++primesFound)
		prime = nextPrime(prime);

	// call controller when done
	auto contr = controller.lock();
	assert(contr);
	contr->update(*this);
}

// prime controller functions
// update info when calculator is done
void Controller::update(const Calculator& calculator) {
	// lock mutex
	updateMutex.lock();

	// update inside info
	++calculatorsDoneNum;
	primesFound += calculator.getResult();
	//DEBUG std::cout << "calculator done: " << calculator.getResult() << std::endl;

	// unlock mutex
	updateMutex.unlock();
}

// find nth prime using threads
int nthPrimeParallel(size_t nth, size_t calculatorsNum = 2) {
	// make initial approximation of nth prime
	int approx = nth * std::log(nth);

	// make controller
	auto controller = std::make_shared<Controller>();

	// calculate calculator range and adjust approximation
	approx -= approx % calculatorsNum;
	size_t range = approx / calculatorsNum;

	// create calculator threads
	std::vector<std::thread> calcThreads;
	for(size_t j = 0; j < calculatorsNum ; ++j)
		calcThreads.emplace_back(Calculator(j * range, (j + 1) * range, controller));

	// wait for calculators to be done
	//  NB: meanwhile, the controller is being updated 
	for(auto& thread : calcThreads)
		thread.join();

	// use info collected by controller in combination with approx to quickly calculate nth prime
	size_t primesFound = controller->getPrimesFound();
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
	
	std::cout << nthPrimeParallel(100001, 4) << std::endl;

	return 0;
}
