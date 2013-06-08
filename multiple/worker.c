/*
 * weighttp - a lightweight and simple webserver benchmarking tool
 *
 * Author:
 *     Copyright (c) 2009-2011 Thomas Porzelt
 *
 * License:
 *     MIT, see COPYING file
 */

#include "weighttp.h"

Worker *worker_new(uint8_t id, Config *config, uint16_t num_clients, uint64_t num_requests) {
	Worker *worker;
	uint16_t i;

	worker = W_MALLOC(Worker, 1);
	worker->id = id;
	worker->loop = ev_loop_new(0);
	ev_ref(worker->loop);
	worker->config = config;
	worker->num_clients = num_clients;
	worker->stats.req_todo = num_requests;
	worker->progress_interval = num_requests / 10;

	if (worker->progress_interval == 0)
		worker->progress_interval = 1;

	worker->clients = W_MALLOC(Client*, num_clients);

	//	uint32_t max = (num_requests / num_clients) * 2;
	uint32_t max = num_requests;

	for (i = 0; i < num_clients; i++) {
		if (NULL == (worker->clients[i] = client_new(worker, max)))
			return NULL;
	}

	return worker;
}

void worker_free(Worker *worker) {
	uint16_t i;

	for (i = 0; i < worker->num_clients; i++)
		client_free(worker->clients[i]);

	free(worker->clients);
	free(worker);
}

void *worker_thread(void* arg) {
	uint16_t i;
	Worker *worker = (Worker*)arg;

	/* start all clients */
	for (i = 0; i < worker->num_clients; i++) {
		if (worker->stats.req_started < worker->stats.req_todo)
			client_state_machine(worker->clients[i]);
	}

	/* A worker does its job very soon. So, let's wait until
	   other works are spawn. */
	sleep(1);

	ev_loop(worker->loop, 0);

	ev_loop_destroy(worker->loop);

	return NULL;
}
