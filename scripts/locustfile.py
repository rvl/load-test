from locust import HttpLocust, TaskSet, task
import random
from datetime import datetime

class WebsiteTasks(TaskSet):
    # def on_start(self):
    #     self.client.post("/login", {
    #         "username": "test_user",
    #         "password": ""
    #     })
    
    @task
    def command(self):
        self.client.get("/command")
        
    @task
    def stats(self):
        self.client.post("/measurement", gen_data(), headers={"Content-type": "application/json"})

class WebsiteUser(HttpLocust):
    task_set = WebsiteTasks
    min_wait = 25000
    max_wait = 35000

def gen_data():
    return {
        "time": datetime.now().isoformat(),
        "charge": random.randint(0, 100),
        "max_charge": random.randint(0, 100),
        "charge_rate": random.random() * 200.0 - 100.0,
        "temp": random.random() * 40.0 + 20.0,
    }
