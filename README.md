English | [简体中文](./README.zh-CN.md)

### python-bridge
Python Bridge is an application programming framework built on top of [emacs-epc](https://github.com/kiwanami/emacs-epc), which can be used to build multi-threaded Emacs plugins through Python.

It has been successfully applied to plugins such as [EAF](https://github.com/emacs-eaf/emacs-application-framework), [lsp-bridge](https://github.com/manateelazycat/lsp-bridge), [blink-search](https://github.com/manateelazycat/blink-search/), [mind-wave](https://github.com/manateelazycat/mind-wave/), [popweb](https://github.com/manateelazycat/popweb), [wraplish](https://github.com/manateelazycat/wraplish), [key-echo](https://github.com/manateelazycat/key-echo) and so on.

### Installation
1. Install Python dependencies: epc, sexpdata, six: `pip3 install epc sexpdata six`
2. Download this repository with `git clone`, and replace the load-path path in the configuration below
3. Add the following code to your configuration file ~/.emacs:

```elisp
(add-to-list 'load-path "<path-to-python-bridge>")

(require 'python-bridge)
(python-bridge-enable)
```

### Usage
**Python side reads the value of Emacs variables**:

```python
from utils import *

print(get_emacs_var("emacs-version"))
print(get_emacs_vars(["emacs-version", "emacs-build-system"]))
```

**Python side gets the return value of Emacs functions**:
```python
from utils import *

print(get_emacs_func_result("get-user-emacs-directory"))
```
If the Emacs function has extra parameters, you can directly write them in the subsequent parameters of `get_emacs_func_result`

**Python side displays messages to Emacs Minibuffer**:
```python
from utils import *

message_emacs("hello from python-bridge")
```

**Python side calls Emacs methods**:
```python
from utils import *

eval_in_emacs("message", "hello from python-bridge")
```

**Emacs side gets the return value of Python functions**:
```python
# Define this function in class PythonBridge.
def echo(self, msg1, msg2):
        eval_in_emacs("message", "%s %s", msg1, msg2)
        return 1
```

```elisp
(python-bridge-call--sync "echo" "hello" "world") ; Will return value 1 from Python function
```
Notice: Such Python function shouldn't use sync type python functions who call epc_client.call_sync(). Or there will be deadlock.

**Python multithreading**:
Please Google `Python threading` to learn how to use Python to write multithreaded code.

### Build your own plugins
So far, you have learned how to use python-bridge to handle the mutual calling of Elisp and Python. Next, you can use python-bridge to build your own plugins.

For example, if I want to build a plugin called `python-hello`, here are my common methods:
1. Search for all `python-bridge` keywords in the python-bridge directory and replace them with `python-hello`
2. Search for all `python_bridge` keywords in the python-bridge directory and replace them with `python_hello`
3. Search for all `PythonBridge` keywords in the python-bridge directory and replace them with `PythonHello`
4. Search for all `PYTHONBRIDGE-EPC-SERVER` keywords in the python-bridge directory, replace with `PYTHONHELLO-EPC-SERVER`
5. Change the file name `python-bridge-epc.el` to `python-hello-epc.el`
6. Change the file name `python-bridge.el` to `python-hello.el`
7. Change the file name `python_bridge.py` to `python_hello.py`
8. Finally, `(require 'python-hello)` is enough

The reason for renaming is to hope that plugins built based on `python-bridge` can have their own namespace and do not interfere with each other.

Happy hacking! ;)
