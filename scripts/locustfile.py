from locust import HttpLocust, TaskSet, task
import random
from datetime import datetime
import json

json_headers = {"Content-type": "application/json" }

class WebsiteTasks(TaskSet):
    def on_start(self):
        # rather convoluted login process but meh
        auth = self.client.post("/login", json={
            "username": "test_user",
        }, headers=json_headers)
        username = auth.json()["username"]
        self.client.headers.update({"servant-auth-cookie": username})
    
    @task
    def command(self):
        self.client.get("/command")
        
    @task
    def stats(self):
        self.client.post("/measurement", json=gen_data(), headers=json_headers)

class WebsiteUser(HttpLocust):
    task_set = WebsiteTasks
    min_wait = 25000
    max_wait = 35000

def gen_data():
    return {
        "id": "00000000-0000-0000-0000-000000000000",
        "time": datetime.now().isoformat() + "Z",
        "charge": random.randint(0, 100),
        "max_charge": random.randint(0, 100),
        "charge_rate": random.random() * 200.0 - 100.0,
        "temp": random.random() * 40.0 + 20.0,
    }
