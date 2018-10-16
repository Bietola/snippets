/***********************************************/
/*  _________________________________________  */
/* / "The only multithreading program that I \ */
/* | will ever write in C."                  | */
/* \ By: Stefano Montesi                     / */
/*  -----------------------------------------  */
/*         \   ^__^                            */
/*          \  (oo)\_______                    */
/*             (__)\       )\/\                */
/*                 ||----w |                   */
/*                 ||     ||                   */
/***********************************************/


// things I need
#include <stdio.h>
#include <stdlib.h>
#include <assert.h> 
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>


/*******************************/
/* thread safe stack structure */
/*******************************/
// some constants
#define BUFLEN 1000

// stack type
typedef struct {
	int data[BUFLEN];
	size_t size;
	pthread_mutex_t mutex;
} stack_t;

// initialize stack
void stack_init(stack_t* stack) {
	stack->size = 0;
	pthread_mutex_init(&(stack->mutex), NULL);
}

// unsafe operations: not thread safe
//  push
void stack_unsafe_push(stack_t* stack, int ele) {
	assert(stack->size != BUFLEN && "stack too big");
	++(stack->size);
	stack->data[stack->size - 1] = ele;
}
//  pop
int stack_unsafe_pop(stack_t* stack) {
	assert(stack->size != 0 && "popping empty stack");
	--(stack->size);
	return stack->data[stack->size];
}

// safe operations: thread safe
//  push
void stack_push(stack_t* stack, int ele) {
	pthread_mutex_lock(&(stack->mutex));
	stack_unsafe_push(stack, ele);
	pthread_mutex_unlock(&(stack->mutex));
}
//  pop
int stack_pop(stack_t* stack) {
	pthread_mutex_lock(&(stack->mutex));
	int element = stack_unsafe_pop(stack);
	pthread_mutex_unlock(&(stack->mutex));

	return element;
}


/*****************************************/
/* consumer/producer functions and types */
/*****************************************/
// constants
#define MAX_PRODUCE_VAL 100

// argument structure for consumer/producer model
typedef struct {
	stack_t* data;
	size_t amount;
} pc_args_t;

// global threading stuff
pthread_mutex_t pc_mutex;
sem_t pc_sem;

// produces numbers from 0 to arg1 and puts them into arg0
void* produce(void* args) { 
	// retrieve arguments 
	pc_args_t* arguments = (pc_args_t*)args; 
	stack_t* data = arguments->data;
   	size_t produceAmount = arguments->amount;

	// do it!
	for(; produceAmount > 0; --produceAmount) {
		// do it!!
		stack_push(data, rand() % MAX_PRODUCE_VAL);
		// simulate slow production process
		sleep(1);
		
		// increment resources available
		sem_post(&pc_sem);
	}
}

// pops arg1 numbers from the arg0 stack and prints them to stdout
void* consume(void* args) {
	// retrieve arguments
	pc_args_t* arguments = (pc_args_t*)args;
	stack_t* data = arguments->data;
	size_t consumeAmount = arguments->amount;

	// do it!
	for(; consumeAmount > 0; --consumeAmount) {
		// decrement number of resources available (claim one for processing)
		//  if no resources available -> wait
		sem_wait(&pc_sem);

		// simulate excruciating consumption process
		sleep(1);
		// do it!!
		//  NB mutexes are still needed for printf, even though stack is safe...
		pthread_mutex_lock(&pc_mutex);
		printf("consumed: %d\n", stack_unsafe_pop(data));
		pthread_mutex_unlock(&pc_mutex);
	}
}


/********/
/* MAIN */
/********/
int main() {
	// initialize threading stuff
	pthread_mutex_init(&pc_mutex, NULL);
	sem_init(&pc_sem, 0, 0);

	// make stack to hold data in
	stack_t data;
	stack_init(&data);

	// set production/consumption parameters
	size_t toProduce = 100;
	size_t toConsume = 100;
	size_t producersNum = 10;
	size_t consumersNum = 10;

	// create producer threads
	pthread_t producerThreads[producersNum + 1];
	// main bulk
	{		
		pc_args_t arguments;
		arguments.data = &data;
		arguments.amount = toProduce / producersNum;
		for(size_t j = 0; j < producersNum; ++j) {
			pthread_create(&producerThreads[j], NULL, produce, (void*)(&arguments));
		}
	}
	// leftovers
	if(toProduce % producersNum != 0){
		pc_args_t arguments;
		arguments.data = &data;
		arguments.amount = toProduce % producersNum;
		pthread_create(&producerThreads[producersNum + 1], NULL, produce, (void*)(&arguments));
		++producersNum;
	}

	// create consumer threads
	pthread_t consumerThreads[consumersNum + 1];
	// main bulk
	{		
		pc_args_t arguments;
		arguments.data = &data;
		arguments.amount = toConsume / consumersNum;
		for(size_t j = 0; j < consumersNum; ++j) {
			pthread_create(&consumerThreads[j], NULL, consume, (void*)(&arguments));
		}
	}
	// leftovers
	if(toConsume % consumersNum != 0) {
		pc_args_t arguments;
		arguments.data = &data;
		arguments.amount = toConsume % consumersNum;
		pthread_create(&consumerThreads[consumersNum + 1], NULL, consume, (void*)(&arguments));
		++consumersNum;
	}

	// join all threads at once
	for(size_t j = 0; j < producersNum; ++j)
		pthread_join(producerThreads[j], NULL);
	for(size_t j = 0; j < consumersNum; ++j)
		pthread_join(consumerThreads[j], NULL);
	
	return 0;
}
