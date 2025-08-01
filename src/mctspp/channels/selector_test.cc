#include <atomic>
#include <cassert>
#include <chrono>
#include <iostream>
#include <random>
#include <thread>

#include "channel.h"

void log(const std::string& message) {
  printf("[%llu] %s\n",
         static_cast<unsigned long long>(
             std::hash<std::thread::id>{}(std::this_thread::get_id())),
         message.c_str());
}

void int_producer(Channel<int>& ch, int id, int count) {
  for (int i = 0; i < count; ++i) {
    std::this_thread::sleep_for(std::chrono::milliseconds(rand() % 500));
    int value = id * 1000 + i;
    bool sent = ch.try_send(value);
    assert(sent && "Int Producer failed to send");
    log("Int Producer " + std::to_string(id) +
        " sent: " + std::to_string(value));
  }
}

void string_producer(Channel<std::string>& ch, int id, int count) {
  for (int i = 0; i < count; ++i) {
    std::this_thread::sleep_for(std::chrono::milliseconds(rand() % 500));
    std::string value =
        "Message " + std::to_string(id) + "-" + std::to_string(i);
    bool sent = ch.try_send(value);
    assert(sent && "String Producer failed to send");
    log("String Producer " + std::to_string(id) + " sent: " + value);
  }
}

void consumer(Channel<int>& ch_int, Channel<std::string>& ch_str,
              Selector& selector) {
  int int_count = 0;
  int str_count = 0;

  selector.add_receive<int>(ch_int, [&int_count](int value) {
    log("Received int: " + std::to_string(value));
    int_count++;
  });

  selector.add_receive<std::string>(ch_str,
                                    [&str_count](const std::string& value) {
                                      log("Received string: " + value);
                                      str_count++;
                                    });

  while (!ch_int.is_closed() || !ch_str.is_closed()) {
    selector.select();
  }

  log("Consumer stopped");

  assert(int_count == 40 && "Incorrect number of int messages received");
  //   assert(str_count == 20 && "Incorrect number of string messages
  //   received");
}

int main() {
  Channel<int> ch_int(5);
  Channel<std::string> ch_str(5);

  Selector selector;
  std::thread cons(
      [&ch_int, &ch_str, &selector] { consumer(ch_int, ch_str, selector); });

  std::thread prod1([&ch_int] { int_producer(ch_int, 1, 20); });
  std::thread prod2([&ch_int] { int_producer(ch_int, 2, 20); });
  std::thread prod3([&ch_str] { string_producer(ch_str, 3, 20); });

  prod1.join();
  prod2.join();
  prod3.join();

  ch_int.close();
  ch_str.close();

  assert(ch_int.is_closed() && "Int channel not closed");
  assert(ch_str.is_closed() && "String channel not closed");

  selector.stop();

  cons.join();

  return 0;
}