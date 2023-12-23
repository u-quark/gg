def pytest_addoption(parser):
    parser.addoption("--add-screenshots", action="store_true", help="Don't check screenshots, write all screenshot files.")
