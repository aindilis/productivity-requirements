:- discontiguous broken/1, completed/1, consumePerDay/3,
dependsCapabilityOnService/2, eventually/1, exists/2, hasBillAmount/2,
hasCapability/1, hasCrisis/2, hasCurrent/1, hasDistraction/2,
hasInterruption/2, hasInventory/2, hasInventory/3,
hasPositionalStrenth/1, hasPositionalWeakness/1, hasProperty/2,
hasReliability/2, hasService/2, hasSomethingForPersonToDo/2,
hasStressor/2, houseHasFunction/1, isa/2, lastReplaced/2, neg/1,
numberOfDaysOfServiceRemaining/2, numberOfDaysWhereReplete/2,
numberOfMonthsOfServiceRemaining/2, possesses/2, sometimes/1,
somewhat/1, today/1, unknownWhether/1, unless/2, wantsToRecreateNow/1,
wasThrownOut/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

today([2019-05-05]).

%% hasReserveInventory(svreStorageRoom1,paperTowelRoll,2).
%% hasReserveInventory(svreStorageRoom1,boxFn(trashBags),1).

%% hasInventory(fridge1,waterBottle,X).
%% hasInventory(topOf(fridge1),waterBottle,0).

%% hasInventory(homeUpstairsFridge,rockstarEnergyDrink,0).

%% hasInventory(home,boxes(sugarFreeEnergyDrinkMix),0).
%% hasInventory(home,boxes(sugarFreeDrinkMix),about(6)).

neg(hasInterruption(andrewDougherty,reactTo(getting(redacted)))).
hasInterruption(andrewDougherty,reactTo(getting(medications))).
neg(hasInterruption(andrewDougherty,reactTo(getting(shelter)))).

%% numberOfDaysWhereReplete([2017-11-18],0).

hasInventory(andrewDougherty,cash,dollars(-1)).
hasInventory(andrewDougherty,paypal,dollars(-1)).
hasInventory(andrewDougherty,serveCard,dollars(-1)).
hasService(internet,indefinitely).

%% neg(responsibleForBill(internet)).

hasBillAmount(internet,unknown(dollars)).
%% %% FIXME: calculate numberOfDaysOfServiceRemaining for internet from date.
%% numberOfDaysOfServiceRemaining(internet,[2017-11-18],unknown(days)).
hasService(electricity,indefinitely).
lastBillAmount(electricity,dollars(-1)).
hasService(gas,indefinitely).
hasService(water,indefinitely).
neg(hasService(drinkingWater)).
hasReliability(electricity,0.999).
hasCapability(airConditioning).
hasCapability(heating).
dependsCapabilityOnService(airConditioning,electricity).
dependsCapabilityOnService(heating,gas).
hasCapability(bathroom).
%% dependsCapabilityOnService(bathroom,rent).
dependsCapabilityOnService(bathroom,water).

%% neg(hasCapability(Item,_)) :-
%% 	broken(Item).
%% %% broken(g930Headset).
%% neg(hasCapability(Item)) :-
%% dependsCapabilityOnService(g930Headset,electricity).
%% charged(g930Headset,false).

sometimes(hasAbility(andrewDougherty,workOnAI)).
neg(hasProperty(andrewDougherty,freeFromDistractions)).
neg(hasProperty(andrewDougherty,sick)).
hasProperty(andrewDougherty,tired).
neg(hasCurrent(airFilter)).
lastReplaced(airFilter,unknown(date)).
neg(hasCrisis(andrewDougherty,moving)).

somewhat(desiresToWorkOn(andrewDougherty,freeLifePlanner)).
neg(hasClearConceptionOfWhatNeedsToBeWorkedOn(andrewDougherty)).
neg(hasInventory(new(imakSmartGloves))).
hasInventory(old(imakSmartGloves)).

hasInventory(empty(jugs5Gallon),3).
hasInventory(full(jugs5Gallon),0).
hasInventory(partial(jugs5Gallon),1).

somewhat(hasProperty(andrewDougherty,healthy)).
neg(feelingMotivated(andrewDougherty)).
hasProperty(andrewDoughertysCellPhone,onTheCharger).

eventually(again(desiresToWorkOn(andrewDougherty,lifePlannerQuarantinedMachine))).
eventually(again(desiresToWorkOn(andrewDougherty,planlogicIntegrationMachine))).
houseHasFunction(lights).
neg(hasPositionalWeakness(broken(bluetoothHeadset))).

hasPositionalWeakness(broken(workhorse)).
neg(hasPositionalWeakness(neg(hasCapability(mail)))).

supplyChainIsSecure(andrewDougherty).