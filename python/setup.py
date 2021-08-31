#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup

def get_README():
    content = ""
    with open("README.md") as f:
        content += f.read()
    return content

setup(
    name="argo-client",
    python_requires=">=3.7",
    version="0.0.8",
    url="https://github.com/GaloisInc/argo",
    project_urls={
        "Changelog": "https://github.com/GaloisInc/argo/blob/master/python/CHANGELOG.md",
        "Source": "https://github.com/GaloisInc/argo/tree/master/python",
        "Bug Tracker": "https://github.com/GaloisInc/argo/issues"
    },
    license="BSD",
    description="A JSON RPC client library.",
    long_description=get_README(),
    long_description_content_type="text/markdown",
    author="Galois, Inc.",
    author_email="andrew@galois.com",
    packages=["argo_client"],
    package_data={"argo_client": ["py.typed"]},
    zip_safe=False,
    install_requires=[
        "requests",
        "mypy"
    ],
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: BSD License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9"
    ],
)
