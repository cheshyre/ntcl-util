! Auto-generated -- DO NOT MODIFY
program unittest
    use :: util_api, only : &
            assert, &
            selector

    use :: string_package_test_module, only : &
            string_package_test
    use :: property_package_test_module, only : &
            property_package_test
    use :: dictionary_package_test_module, only : &
            dictionary_package_test
    use :: measurement_package_test_module, only : &
            measurement_package_test
    use :: quantum_number_package_test_module, only : &
            quantum_number_package_test
    use :: commandline_package_test_module, only : &
            commandline_package_test
    use :: configuration_package_test_module, only : &
            configuration_package_test
    use :: iterator_package_test_module, only : &
            iterator_package_test
    use :: iterator_builder_package_test_module, only : &
            iterator_builder_package_test
    use :: iterator_constraints_package_test_module, only : &
            iterator_constraints_package_test
    use :: domain_package_test_module, only : &
            domain_package_test
    use :: domain_builder_package_test_module, only : &
            domain_builder_package_test
    use :: interoptability_package_test_module, only : &
            interoptability_package_test
    use :: assert_package_test_module, only : &
            assert_package_test
    use :: timer_package_test_module, only : &
            timer_package_test
    use :: readers_package_test_module, only : &
            readers_package_test

    implicit none

    type(assert) :: assertion
    type(selector) :: aselector

    type(string_package_test) :: &
            astring_package_test
    type(property_package_test) :: &
            aproperty_package_test
    type(dictionary_package_test) :: &
            adictionary_package_test
    type(measurement_package_test) :: &
            ameasurement_package_test
    type(quantum_number_package_test) :: &
            aquantum_number_package_test
    type(commandline_package_test) :: &
            acommandline_package_test
    type(configuration_package_test) :: &
            aconfiguration_package_test
    type(iterator_package_test) :: &
            aiterator_package_test
    type(iterator_builder_package_test) :: &
            aiterator_builder_package_test
    type(iterator_constraints_package_test) :: &
            aiterator_constraints_package_test
    type(domain_package_test) :: &
            adomain_package_test
    type(domain_builder_package_test) :: &
            adomain_builder_package_test
    type(interoptability_package_test) :: &
            ainteroptability_package_test
    type(assert_package_test) :: &
            aassert_package_test
    type(timer_package_test) :: &
            atimer_package_test
    type(readers_package_test) :: &
            areaders_package_test

    assertion = assert()
    aselector = selector()

    astring_package_test = string_package_test(aselector)
    call astring_package_test%run(assertion)
    call astring_package_test%cleanup()

    aproperty_package_test = property_package_test(aselector)
    call aproperty_package_test%run(assertion)
    call aproperty_package_test%cleanup()

    adictionary_package_test = dictionary_package_test(aselector)
    call adictionary_package_test%run(assertion)
    call adictionary_package_test%cleanup()

    ameasurement_package_test = measurement_package_test(aselector)
    call ameasurement_package_test%run(assertion)
    call ameasurement_package_test%cleanup()

    aquantum_number_package_test = quantum_number_package_test(aselector)
    call aquantum_number_package_test%run(assertion)
    call aquantum_number_package_test%cleanup()

    acommandline_package_test = commandline_package_test(aselector)
    call acommandline_package_test%run(assertion)
    call acommandline_package_test%cleanup()

    aconfiguration_package_test = configuration_package_test(aselector)
    call aconfiguration_package_test%run(assertion)
    call aconfiguration_package_test%cleanup()

    aiterator_package_test = iterator_package_test(aselector)
    call aiterator_package_test%run(assertion)
    call aiterator_package_test%cleanup()

    aiterator_builder_package_test = iterator_builder_package_test(aselector)
    call aiterator_builder_package_test%run(assertion)
    call aiterator_builder_package_test%cleanup()

    aiterator_constraints_package_test = iterator_constraints_package_test(aselector)
    call aiterator_constraints_package_test%run(assertion)
    call aiterator_constraints_package_test%cleanup()

    adomain_package_test = domain_package_test(aselector)
    call adomain_package_test%run(assertion)
    call adomain_package_test%cleanup()

    adomain_builder_package_test = domain_builder_package_test(aselector)
    call adomain_builder_package_test%run(assertion)
    call adomain_builder_package_test%cleanup()

    ainteroptability_package_test = interoptability_package_test(aselector)
    call ainteroptability_package_test%run(assertion)
    call ainteroptability_package_test%cleanup()

    aassert_package_test = assert_package_test(aselector)
    call aassert_package_test%run(assertion)
    call aassert_package_test%cleanup()

    atimer_package_test = timer_package_test(aselector)
    call atimer_package_test%run(assertion)
    call atimer_package_test%cleanup()

    areaders_package_test = readers_package_test(aselector)
    call areaders_package_test%run(assertion)
    call areaders_package_test%cleanup()

    call assertion%write_summary()

    call aselector%cleanup()
    call assertion%cleanup()
end program unittest
