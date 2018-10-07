import CPUTest as CPUTest
import BusTest as BusTest
import EasyTest

main = run $ tests [CPUTest.test, BusTest.test]
