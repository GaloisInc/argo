from setuptools import setup, find_packages

setup(name='argo',
      version='0.0.1',
      packages=['argo', 'cryptol', 'saw'],
      package_data={
          '': ['py.typed']
      }
)
