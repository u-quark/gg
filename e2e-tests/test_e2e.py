#!python

import subprocess
import shutil
from os import chdir, environ, remove, chmod, symlink
import stat
from pathlib import Path
from tempfile import TemporaryDirectory
from time import sleep
import hecate.hecate as hecate
import pytest

def git(*args, repo, **env):
    std_env = {
        "GIT_COMMITTER_NAME": "test",
        "GIT_COMMITTER_EMAIL": "test@mail.com",
        "GIT_COMMITTER_DATE": "1970-01-01T00:00:00.000+00:00",
        "GIT_AUTHOR_NAME": "test",
        "GIT_AUTHOR_EMAIL": "test@mail.com",
        "GIT_AUTHOR_DATE": "1970-01-01T00:00:00.000+00:00",
        "HOME": str(repo),
        "GIT_CONFIG_NOSYSTEM": "1",
    }
    actual_env = environ.copy()
    actual_env.update(std_env)
    if env:
        actual_env.update(env)
    try:
        subprocess.run(
            ("git",) + args,
            cwd=repo,
            env=actual_env,
            capture_output=True,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as ex:
        pytest.fail(
            f"Git command failed with exit code {ex.returncode}\n"
            f"stdout:\n{ex.stdout}\nstderr:\n{ex.stderr}")

@pytest.fixture
def tmp_repo(tmp_path):
    with open(tmp_path / ".gitconfig", "w") as fd:
        fd.write("""
[user]
    name = test
    email = test@main.com
[init]
    defaultBranch = main
        """)
    git("init", repo=tmp_path)
    git("commit", "-m", "Initial commit", "--allow-empty", repo=tmp_path)
    yield tmp_path

def get_runner(tmp_repo):
    gg_path = str(Path(environ["GG_PATH"]).resolve().absolute())
    with open(tmp_repo / ".tmux.conf", "w") as fd:
        fd.write("""
set-option -g mouse on
        """)
    runner = hecate.Runner(
        gg_path,
        width=120, height=36,
        env={
            "HOME": tmp_repo,
            "GIT_CONFIG_NOSYSTEM": "1",
            "GIT_COMMITTER_DATE": "1970-01-01T00:00:00.000+00:00",
            "TERM": "tmux-256color",
            "COLORTERM": "truecolor",
        },
        cwd=str(tmp_repo),
        tmux_conf=tmp_repo / ".tmux.conf",
    )
    sleep(0.2)
    return runner

class ScreenshotChecker:
    def __init__(self, test_name, add_screenshots):
        self.test_name = test_name
        self.add_screenshots = add_screenshots
        self.count = 0

    def check(self, runner):
        __tracebackhide__ = True
        sleep(0.1)
        screenshot = runner.screenshot_with_escape_sequences()
        self.count += 1
        screenshots_dir = Path("./screenshots")
        filename = f"{self.test_name}_{self.count:03}"
        golden_filepath = screenshots_dir / filename
        new_filepath = screenshots_dir / f"{filename}_new"
        if self.add_screenshots:
            with open(golden_filepath, "w") as fd:
                fd.write(screenshot)
            print(f"Saved screenshot to {golden_filepath}")
            return
        if not golden_filepath.exists():
            with open(golden_filepath, "w") as fd:
                fd.write(screenshot)
            pytest.fail(f"Missing screenshot #{self.count:03} file: {golden_filepath}")
        with open(golden_filepath) as fd:
            golden_screenshot = fd.read()
        if screenshot != golden_screenshot:
            print(f"Golden:\n{golden_screenshot}\x1b[0m")
            print(f"New:\n{screenshot}\x1b[0m")
            with open(new_filepath, "w") as fd:
                fd.write(screenshot)
            print(f"Saved new screenshot to {new_filepath}")
            pytest.fail(f"Screenshot #{self.count:03} differs, test: {self.test_name}")

def escape_sequence(sequence):
    return ["-H", "1b"] + list(f"{ord(s):x}" for s in sequence)

def mouse_wheel_down(x, y):
    return escape_sequence(f"[<65;{x};{y}M")

def mouse_wheel_up(x, y):
    return escape_sequence(f"[<64;{x};{y}M")

@pytest.fixture
def screenshot_checker(request):
    test_name = request.node.name
    add_screenshots = request.config.getoption("--add-screenshots")
    yield ScreenshotChecker(test_name, add_screenshots)

def test_basic_functionality(tmp_repo, screenshot_checker):
    with open(tmp_repo / "a", "w") as fd:
        fd.write("a")
    git("add", "a", repo=tmp_repo)
    git("commit", "-m", "A", repo=tmp_repo)
    with open(tmp_repo / "b", "w") as fd:
        fd.write("b")
    git("add", "b", repo=tmp_repo)
    git(
        "commit", "-m", "B",
        repo=tmp_repo,
        GIT_AUTHOR_NAME="test1",
        GIT_AUTHOR_EMAIL="test1@mail.com",
        GIT_AUTHOR_DATE="2023-11-11T05:17:00.000+02:00",
    )
    with get_runner(tmp_repo) as runner:
        screenshot_checker.check(runner)
        runner.press("enter")
        screenshot_checker.check(runner)
        runner.press("escape")
        screenshot_checker.check(runner)
        runner.press("j")
        screenshot_checker.check(runner)
        runner.press("enter")
        screenshot_checker.check(runner)
        runner.press("q")
        runner.press("q")
        runner.await_exit()

def test_navigation(tmp_repo, screenshot_checker):
    for i in range(100):
        git("commit", "-m", f"commit {i}", "--allow-empty", repo=tmp_repo)
    with open(tmp_repo / "test", "w") as fd:
        for i in range(100):
            fd.write(f"test {i}\n")
    git("add", "test", repo=tmp_repo)
    git("commit", "-m", "test", repo=tmp_repo)
    with get_runner(tmp_repo) as runner:
        # Enter commit diff
        screenshot_checker.check(runner)
        runner.press("enter")
        screenshot_checker.check(runner)
        # Viewport scrolling
        runner.press("down")
        screenshot_checker.check(runner)
        runner.press("up")
        screenshot_checker.check(runner)
        runner.press("pagedown")
        screenshot_checker.check(runner)
        runner.press("pageup")
        screenshot_checker.check(runner)
        runner.press("end")
        screenshot_checker.check(runner)
        runner.press("home")
        screenshot_checker.check(runner)
        runner.press("j")
        screenshot_checker.check(runner)
        runner.press("k")
        screenshot_checker.check(runner)
        runner.press("c-f")
        screenshot_checker.check(runner)
        runner.press("c-b")
        screenshot_checker.check(runner)
        runner.press("G")
        screenshot_checker.check(runner)
        runner.press("g")
        screenshot_checker.check(runner)
        runner.press(*mouse_wheel_down(1, 22))
        screenshot_checker.check(runner)
        runner.press(*mouse_wheel_up(1, 22))
        screenshot_checker.check(runner)
        # Exit commit diff
        runner.press("q")
        screenshot_checker.check(runner)
        # Commit list scrolling
        runner.press("down")
        screenshot_checker.check(runner)
        runner.press("pagedown")
        screenshot_checker.check(runner)
        runner.press("end")  # NO-OP
        screenshot_checker.check(runner)
        runner.press("home")
        screenshot_checker.check(runner)
        runner.press("j")
        screenshot_checker.check(runner)
        runner.press("c-f")
        screenshot_checker.check(runner)
        runner.press("G")  # NO-OP
        screenshot_checker.check(runner)
        runner.press("g")
        screenshot_checker.check(runner)
        runner.press(*mouse_wheel_down(1, 1))
        screenshot_checker.check(runner)
        runner.press(*mouse_wheel_up(1, 1))
        screenshot_checker.check(runner)
        # Exit
        runner.press("q")
        runner.await_exit()

def test_rebase_actions(tmp_repo, screenshot_checker):
    with open(tmp_repo / "test2", "w") as fd:
        fd.write("test 2\n")
    git("add", "test2", repo=tmp_repo)
    git("commit", "-m", "Test 2", repo=tmp_repo)
    with open(tmp_repo / "test1", "w") as fd:
        fd.write("test 1\n")
    git("add", "test1", repo=tmp_repo)
    git("commit", "-m", "Test 1", repo=tmp_repo)
    with get_runner(tmp_repo) as runner:
        screenshot_checker.check(runner)
        runner.press("K")  # Move commit #1 up: NO-OP, indicator shown
        screenshot_checker.check(runner)
        runner.press("Z")  # Undo: NO-OP, indicator shown
        screenshot_checker.check(runner)
        runner.press("J")  # Move commit #1 down
        screenshot_checker.check(runner)
        runner.press("K")  # Move commit #1 up
        screenshot_checker.check(runner)
        runner.press("S")  # Squash commit #1 into #2
        runner.press("enter")  # open commit #2
        screenshot_checker.check(runner)
        runner.press("q")
        runner.press("Z")  # Undo last squash
        screenshot_checker.check(runner)
        runner.press("F")  # Fixup commit #1 into #2
        runner.press("enter")  # open commit #2
        screenshot_checker.check(runner)
        runner.press("q")
        runner.press("Z")  # Undo last fixup
        screenshot_checker.check(runner)
        runner.press("D")  # Delete commit #1
        screenshot_checker.check(runner)
        runner.press("Z")  # Undo last delete
        screenshot_checker.check(runner)
        runner.press("R")  # Redo last delete
        screenshot_checker.check(runner)
        runner.press("R")  # Redo again: NO-OP, indicator shown
        screenshot_checker.check(runner)
        # Exit
        runner.press("q")
        runner.await_exit()

def test_diff(tmp_repo, screenshot_checker):
    def take_diff_screenshot():
        __tracebackhide__ = True
        with get_runner(tmp_repo) as runner:
            runner.press("enter")
            screenshot_checker.check(runner)
            runner.press("q")
            runner.press("q")
            runner.await_exit()

    with open(tmp_repo / "test_a", "w") as fd:
        fd.write("line 1\n")
        fd.write("line 2\n")
        fd.write("line 3\n")
    git("add", "test_a", repo=tmp_repo)
    git("commit", "-m", "Add test_a", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_a", "w") as fd:
        fd.write("line 1\n")
        fd.write("mod line\n")
        fd.write("line 3\n")
    git("add", "test_a", repo=tmp_repo)
    git("commit", "-m", "Modify test_a: modify line", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_a", "a") as fd:
        fd.write("line 4\n")
        fd.write("line 4\n")
    git("add", "test_a", repo=tmp_repo)
    git("commit", "-m", "Modify test_a: add lines", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_a", "w") as fd:
        fd.write("line 1\n")
        fd.write("line 2\n")
        fd.write("line 3\n")
        fd.write("line 4\n")
    git("add", "test_a", repo=tmp_repo)
    git("commit", "-m", "Modify test_a: delete and modify lines", repo=tmp_repo)
    take_diff_screenshot()
    shutil.copyfile(tmp_repo / "test_a", tmp_repo / "test_a_copy")
    git("add", "test_a_copy", repo=tmp_repo)
    git("commit", "-m", "Copy test_a to test_a_copy", repo=tmp_repo)
    take_diff_screenshot()
    git("rm", "test_a_copy", repo=tmp_repo)
    git("commit", "-m", "Delete test_a_copy", repo=tmp_repo)
    take_diff_screenshot()
    shutil.copyfile(tmp_repo / "test_a", tmp_repo / "test_a_copy_2")
    with open(tmp_repo / "test_a_copy_2", "a") as fd:
        fd.write("line 5\n")
    git("add", "test_a_copy_2", repo=tmp_repo)
    git("commit", "-m", "Copy test_a to test_a_copy_2 with modifications", repo=tmp_repo)
    take_diff_screenshot()
    git("rm", "test_a_copy_2", repo=tmp_repo)
    git("commit", "-m", "Delete test_a_copy_2", repo=tmp_repo)
    take_diff_screenshot()
    shutil.copyfile(tmp_repo / "test_a", tmp_repo / "test_a_copy_3")
    with open(tmp_repo / "test_a_copy_3", "a") as fd:
        fd.write("line 6\n")
    with open(tmp_repo / "test_a", "a") as fd:
        fd.write("line 7\n")
    git("add", "test_a", "test_a_copy_3", repo=tmp_repo)
    git("commit", "-m", "Copy test_a to test_a_copy_3 with both modified", repo=tmp_repo)
    take_diff_screenshot()
    git("rm", "test_a_copy_3", repo=tmp_repo)
    git("commit", "-m", "Delete test_a_copy_3", repo=tmp_repo)
    take_diff_screenshot()
    git("mv", "test_a", "test_b", repo=tmp_repo)
    git("commit", "-m", "Rename test_a to test_b", repo=tmp_repo)
    take_diff_screenshot()
    git("mv", "test_b", "test_c", repo=tmp_repo)
    with open(tmp_repo / "test_c", "a") as fd:
        fd.write("line 5\n")
    git("add", "test_c", repo=tmp_repo)
    git("commit", "-m", "Rename test_b to test_c with modifications", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_exec", "w") as fd:
        fd.write("test\n")
    chmod(tmp_repo / "test_exec", 0o766)
    git("add", "test_exec", repo=tmp_repo)
    git("commit", "-m", "Add executable test_exec", repo=tmp_repo)
    take_diff_screenshot()
    chmod(tmp_repo / "test_exec", 0o666)
    git("add", "test_exec", repo=tmp_repo)
    git("commit", "-m", "Remove executable flag from test_exec", repo=tmp_repo)
    take_diff_screenshot()
    chmod(tmp_repo / "test_exec", 0o766)
    git("add", "test_exec", repo=tmp_repo)
    git("commit", "-m", "Add executable flag to test_exec", repo=tmp_repo)
    take_diff_screenshot()
    git("rm", "test_exec", repo=tmp_repo)
    git("commit", "-m", "Delete executable test_exec", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_bin", "wb") as fd:
        fd.write(b"\0")
    git("add", "test_bin", repo=tmp_repo)
    git("commit", "-m", "Add binary test_bin", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_bin", "ab") as fd:
        fd.write(b"\x01")
    git("add", "test_bin", repo=tmp_repo)
    git("commit", "-m", "Modify binary test_bin", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_bin", "w") as fd:
        fd.write("text\n")
    git("add", "test_bin", repo=tmp_repo)
    git("commit", "-m", "Change from binary to text test_bin", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_bin", "wb") as fd:
        fd.write(b"\0")
    git("add", "test_bin", repo=tmp_repo)
    git("commit", "-m", "Change from text to binary test_bin", repo=tmp_repo)
    take_diff_screenshot()
    git("rm", "test_bin", repo=tmp_repo)
    git("commit", "-m", "Delete binary test_bin", repo=tmp_repo)
    take_diff_screenshot()
    symlink("./test_a", tmp_repo / "test_link")
    git("add", "test_link", repo=tmp_repo)
    git("commit", "-m", "Add symlink test_link", repo=tmp_repo)
    take_diff_screenshot()
    remove(tmp_repo / "test_link")
    symlink("./test_b", tmp_repo / "test_link")
    git("add", "test_link", repo=tmp_repo)
    git("commit", "-m", "Modify symlink test_link", repo=tmp_repo)
    take_diff_screenshot()
    remove(tmp_repo / "test_link")
    with open(tmp_repo / "test_link", "w") as fd:
        fd.write("test\n")
    git("add", "test_link", repo=tmp_repo)
    git("commit", "-m", "Change from symlink to regular file test_link", repo=tmp_repo)
    take_diff_screenshot()
    remove(tmp_repo / "test_link")
    symlink("./test_a", tmp_repo / "test_link")
    git("add", "test_link", repo=tmp_repo)
    git("commit", "-m", "Change from regular file to symlink test_link", repo=tmp_repo)
    take_diff_screenshot()
    git("rm", "test_link", repo=tmp_repo)
    git("commit", "-m", "Delete symlink test_link", repo=tmp_repo)
    take_diff_screenshot()
    git("commit", "--allow-empty", "-m", "An empty commit", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_empty", "w") as fd:
        pass
    git("add", "test_empty", repo=tmp_repo)
    git("commit", "-m", "Add empty file", repo=tmp_repo)
    take_diff_screenshot()
    git(
        "commit", "--allow-empty", "-m", "Committer name is different to the author's",
        repo=tmp_repo, GIT_COMMITTER_NAME="test2",
    )
    take_diff_screenshot()
    git(
        "commit", "--allow-empty", "-m", "Committer email is different to the author's",
        repo=tmp_repo, GIT_COMMITTER_EMAIL="test2@mail.com",
    )
    take_diff_screenshot()
    git(
        "commit", "--allow-empty", "-m", "Committer date is different to the author's",
        repo=tmp_repo, GIT_COMMITTER_DATE="1980-01-01 00:00",
    )
    take_diff_screenshot()
    git(
        "commit", "--allow-empty", "-m", "All committer details are different to the author's",
        repo=tmp_repo,
        GIT_COMMITTER_NAME="test2", GIT_COMMITTER_EMAIL="test2@mail.com", GIT_COMMITTER_DATE="1980-01-01 00:00",
    )
    take_diff_screenshot()
    with open(tmp_repo / "test_newline", "w") as fd:
        fd.write("test")
    git("add", "test_newline", repo=tmp_repo)
    git("commit", "-m", "Add file test_newline without a newline at the EOF", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_newline", "w") as fd:
        fd.write("test\n")
    git("add", "test_newline", repo=tmp_repo)
    git("commit", "-m", "Add a newline at the end of file test_newline", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_newline", "w") as fd:
        fd.write("test")
    git("add", "test_newline", repo=tmp_repo)
    git("commit", "-m", "Remove newline from the end of file test_newline", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_newline", "w") as fd:
        fd.write("modified")
    git("add", "test_newline", repo=tmp_repo)
    git("commit", "-m", "Modify last line of a file test_newline", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test_newline", "w") as fd:
        fd.write("added line\n")
        fd.write("modified")
    git("add", "test_newline", repo=tmp_repo)
    git("commit", "-m", "Add a line to the top of file test_newline", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test.c", "w") as fd:
        fd.write("int foo() {\n")
        for i in range(10):
            fd.write(f"    print(\"test {i}\");\n")
        fd.write("}\n")
    git("add", "test.c", repo=tmp_repo)
    git("commit", "-m", "Add C file test.c", repo=tmp_repo)
    take_diff_screenshot()
    with open(tmp_repo / "test.c", "w") as fd:
        fd.write("int foo() {\n")
        for i in range(10):
            if i == 8:
                fd.write("    print(\"test modified\");\n")
            else:
                fd.write(f"    print(\"test {i}\");\n")
        fd.write("}\n")
    git("add", "test.c", repo=tmp_repo)
    git("commit", "-m", "Modify C file test.c within function foo", repo=tmp_repo)
    take_diff_screenshot()
