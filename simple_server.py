#!/usr/bin/env python3
"""
Simple HTTP server to demonstrate the concept while Haskell builds.
This implements the same API with proper HTTP methods and effects logging.
"""

import json
import threading
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs

# Global state
number_state = {'value': 0}
state_lock = threading.Lock()

# HTTP method log (simulates our "Safe" grade logging)
http_log = []

def log_request(method, path):
    """Safe effect: logging is non-observable to client"""
    http_log.append(f"{method} {path}")
    print(f"HTTP Log: {method} {path}")

class NumberHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        parsed = urlparse(self.path)
        log_request("GET", parsed.path)
        
        if parsed.path == '/':
            self.serve_html()
        elif parsed.path == '/show':
            self.handle_show()
        else:
            self.send_error(404)
    
    def do_PUT(self):
        log_request("PUT", self.path)
        if self.path == '/set':
            self.handle_set()
        else:
            self.send_error(405, "Method Not Allowed")
    
    def do_POST(self):
        log_request("POST", self.path)
        if self.path == '/add':
            self.handle_add()
        else:
            self.send_error(405, "Method Not Allowed")
    
    def serve_html(self):
        html = """<!DOCTYPE html>
<html>
<head>
    <title>I'm Thinking of a Number</title>
</head>
<body>
    <h1>I'm Thinking of a Number</h1>
    
    <div>
        <input type="number" id="setValue" placeholder="Enter number">
        <button onclick="setNumber()">Set</button>
    </div>
    
    <div>
        <input type="number" id="addValue" placeholder="Enter number">
        <button onclick="addNumber()">Add</button>
    </div>
    
    <div>
        <button onclick="showNumber()">Show</button>
    </div>
    
    <div id="result"></div>
    
    <script>
        function setNumber() {
            const value = document.getElementById('setValue').value;
            fetch('/set', {
                method: 'PUT',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({value: parseInt(value)})
            })
            .then(response => response.json())
            .then(data => {
                document.getElementById('result').innerText = 'Set to: ' + data.value;
            });
        }
        
        function addNumber() {
            const value = document.getElementById('addValue').value;
            fetch('/add', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({value: parseInt(value)})
            })
            .then(response => response.json())
            .then(data => {
                document.getElementById('result').innerText = 'Result: ' + data.value;
            });
        }
        
        function showNumber() {
            fetch('/show')
            .then(response => response.json())
            .then(data => {
                document.getElementById('result').innerText = 'Current number: ' + data.value;
            });
        }
    </script>
</body>
</html>"""
        self.send_response(200)
        self.send_header('Content-Type', 'text/html')
        self.end_headers()
        self.wfile.write(html.encode())
    
    def handle_show(self):
        """GET /show - Safe operation"""
        with state_lock:
            result = {'value': number_state['value']}
        
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.wfile.write(json.dumps(result).encode())
    
    def handle_set(self):
        """PUT /set - Idempotent operation"""
        content_length = int(self.headers['Content-Length'])
        body = self.rfile.read(content_length)
        data = json.loads(body)
        
        with state_lock:
            number_state['value'] = data['value']
            result = {'value': number_state['value']}
        
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.wfile.write(json.dumps(result).encode())
    
    def handle_add(self):
        """POST /add - Unsafe operation"""
        content_length = int(self.headers['Content-Length'])
        body = self.rfile.read(content_length)
        data = json.loads(body)
        
        with state_lock:
            number_state['value'] += data['value']
            result = {'value': number_state['value']}
        
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.wfile.write(json.dumps(result).encode())

if __name__ == '__main__':
    server = HTTPServer(('localhost', 8080), NumberHandler)
    print("Server starting on http://localhost:8080")
    print("This demonstrates the concept while Haskell builds...")
    server.serve_forever()