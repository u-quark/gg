#!python

import subprocess
from os import chdir, environ
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
    subprocess.run(
        ("git",) + args,
        cwd=repo,
        env=actual_env,
        stdout=subprocess.DEVNULL,
        check=True,
    )

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
    gg_path = Path(environ["GG_PATH"]).resolve().absolute()
    runner = hecate.Runner(
        "sh", "-c", f"cd {tmp_repo}; {gg_path}",
        width=120, height=36,
        env={
            "HOME": tmp_repo,
            "GIT_CONFIG_NOSYSTEM": "1",
            "GIT_COMMITTER_DATE": "1970-01-01T00:00:00.000+00:00",
            "TERM": "tmux-256color",
            "COLORTERM": "truecolor",
        }
    )
    sleep(0.2)
    return runner

class ScreenshotChecker:
    def __init__(self, test_name):
        self.test_name = test_name
        self.count = 0

    def check(self, runner):
        __tracebackhide__ = True
        sleep(0.2)
        screenshot = runner.screenshot_with_escape_sequences()
        self.count += 1
        screenshots_dir = Path("./screenshots")
        filename = f"{self.test_name}_{self.count:03}"
        golden_filepath = screenshots_dir / filename
        new_filepath = screenshots_dir / f"{filename}_new"
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

@pytest.fixture
def screenshot_checker(request):
    test_name = request.node.name
    yield ScreenshotChecker(test_name)

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
