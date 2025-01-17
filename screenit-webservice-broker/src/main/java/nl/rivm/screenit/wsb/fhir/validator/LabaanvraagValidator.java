package nl.rivm.screenit.wsb.fhir.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.text.ParseException;
import java.util.Date;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.util.CervixLocatieUtil;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixHuisartsLocatieService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.wsb.fhir.exception.NotValidatedException;
import nl.rivm.screenit.wsb.fhir.exception.RequiredPropertyException;
import nl.rivm.screenit.wsb.fhir.mapper.LabaanvraagMapper;
import nl.rivm.screenit.wsb.fhir.resource.dstu3.v1.CodeSystem;
import nl.rivm.screenit.wsb.fhir.resource.dstu3.v1.LabaanvraagBundle;
import nl.rivm.screenit.wsb.fhir.resource.dstu3.v1.LabaanvraagResource;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;
import nl.topicuszorg.util.bsn.BsnUtils;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Hibernate;
import org.hl7.fhir.dstu3.model.CodeableConcept;
import org.hl7.fhir.dstu3.model.OperationOutcome;
import org.hl7.fhir.dstu3.model.StringType;

@Slf4j
public class LabaanvraagValidator
{
	private final HibernateService hibernateSerice;

	private final ClientService clientService;

	private final CervixHuisartsLocatieService huisartsLocatieService;

	private final CervixBaseMonsterService monsterService;

	private final CervixBaseScreeningrondeService screeningRondeService;

	private final ICurrentDateSupplier currentDateSupplier;

	private boolean validated;

	private final OperationOutcome operationOutcome = new OperationOutcome();

	public LabaanvraagValidator()
	{
		validated = false;
		clientService = ApplicationContextProvider.getApplicationContext().getBean(ClientService.class);
		huisartsLocatieService = ApplicationContextProvider.getApplicationContext().getBean(CervixHuisartsLocatieService.class);
		monsterService = ApplicationContextProvider.getApplicationContext().getBean(CervixBaseMonsterService.class);
		screeningRondeService = ApplicationContextProvider.getApplicationContext().getBean(CervixBaseScreeningrondeService.class);
		currentDateSupplier = ApplicationContextProvider.getApplicationContext().getBean(ICurrentDateSupplier.class);
		hibernateSerice = ApplicationContextProvider.getApplicationContext().getBean(HibernateService.class);

	}

	public LabaanvraagValidator validate(LabaanvraagResource resource)
	{
		validateClientGerelateerdeData(resource);
		validateHuisartsGerelateerdeData(resource);
		if (resource instanceof LabaanvraagBundle && operationOutcome.isEmpty())
		{
			try
			{
				var bundle = ((LabaanvraagBundle) resource);
				var labformulier = LabaanvraagMapper.INSTANCE.toLabformulier(bundle);
				validateFormMapping(bundle, labformulier);
				validateFormContent(labformulier);
			}
			catch (Exception e)
			{
				if (e.getCause() instanceof ParseException)
				{
					addMappingIssue(e.getCause().getMessage());
				}
				else if (e instanceof IllegalArgumentException)
				{
					addMappingIssue(e.getMessage());
				}
				else
				{
					addMappingIssue(e);
				}
			}
		}
		validated = true;
		return this;
	}

	private void validateFormContent(CervixLabformulier labformulier)
	{
		validateDatumUitstrijkjeContent(labformulier);
		validateKlachtenContent(labformulier);
		validateMenstruatieContent(labformulier);
		validateAnticonceptieContent(labformulier);
		validateHormonenContent(labformulier);
		validateAspectCervixContent(labformulier);
	}

	private void validateAspectCervixContent(CervixLabformulier labformulier)
	{

		if (Stream.of(labformulier.isAspectCervixNietGezien(),
				labformulier.isAspectCervixNormaal(),
				labformulier.isAspectCervixAbnormaalOfVerdachtePortio())
			.filter(el -> el)
			.count() != 1)
		{
			addQuestionIssue(CodeSystem.ASPECT_CERVIX);
		}
	}

	private void validateHormonenContent(CervixLabformulier labformulier)
	{

		if (!labformulier.isGebruikHormonenGeen() &&
			!(labformulier.isGebruikHormonenJaVanwegeBorstkanker() ||
				labformulier.isGebruikHormonenJaVanwegeOvergangsklachten() ||
				labformulier.isGebruikHormonenJaVanwege()))
		{
			addQuestionIssue(CodeSystem.GEBRUIK_HORMONEN);
		}

		else if (labformulier.isGebruikHormonenGeen() &&
			(labformulier.isGebruikHormonenJaVanwegeBorstkanker() ||
				labformulier.isGebruikHormonenJaVanwegeOvergangsklachten() ||
				labformulier.isGebruikHormonenJaVanwege()))
		{
			addQuestionIssue(CodeSystem.GEBRUIK_HORMONEN);
		}

		if (labformulier.isGebruikHormonenJaVanwege() &&
			StringUtils.isBlank(labformulier.getGebruikHormonenJaVanwegeTekst()))
		{
			addQuestionIssue(CodeSystem.GEBRUIK_HORMONEN_VRIJE_TEKST);
		}
		else if (!labformulier.isGebruikHormonenJaVanwege() &&
			!StringUtils.isBlank(labformulier.getGebruikHormonenJaVanwegeTekst()))
		{
			addQuestionIssue(CodeSystem.GEBRUIK_HORMONEN_VRIJE_TEKST);
		}
	}

	private void validateAnticonceptieContent(CervixLabformulier labformulier)
	{
		if (Stream.of(labformulier.isAnticonceptieGeen(),
				labformulier.isAnticonceptiePil(),
				labformulier.isAnticonceptieIudKoper(),
				labformulier.isAnticonceptieIudMirena(),
				labformulier.isAnticonceptieAnders())
			.filter(el -> el)
			.count() != 1)
		{
			addQuestionIssue(CodeSystem.ANTICONCEPTIE);
		}
	}

	private void validateMenstruatieContent(CervixLabformulier labformulier)
	{
		if (Stream.of(labformulier.isMenstruatieNormaal(),
				labformulier.isMenstruatieMenopauze(),
				labformulier.isMenstruatiePostmenopauze(),
				labformulier.isMenstruatieGeenMenstruatie())
			.filter(el -> el)
			.count() != 1)
		{
			addQuestionIssue(CodeSystem.MENSTRUATIE);
		}

		if (labformulier.getDatumLaatsteMenstruatie() != null
			&& currentDateSupplier.getLocalDate().isBefore(DateUtil.toLocalDate(labformulier.getDatumLaatsteMenstruatie())))
		{
			addQuestionIssue(CodeSystem.DATUM_LAATSTE_MENSTRUATIE);
		}
	}

	private void validateKlachtenContent(CervixLabformulier labformulier)
	{

		if (!labformulier.isKlachtenGeen() &&
			!(labformulier.isKlachtenContactbloedingen() ||
				labformulier.isKlachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak() ||
				labformulier.isKlachtenIntermenstrueelBloedverlies() ||
				labformulier.isKlachtenPostmenopauzaalBloedverlies() ||
				labformulier.isKlachtenAndersNamelijk()))
		{
			addQuestionIssue(CodeSystem.KLACHTEN);
		}

		else if (labformulier.isKlachtenGeen() &&
			(labformulier.isKlachtenContactbloedingen() ||
				labformulier.isKlachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak() ||
				labformulier.isKlachtenIntermenstrueelBloedverlies() ||
				labformulier.isKlachtenPostmenopauzaalBloedverlies() ||
				labformulier.isKlachtenAndersNamelijk()))
		{
			addQuestionIssue(CodeSystem.KLACHTEN);
		}

		if (labformulier.isKlachtenAndersNamelijk() &&
			StringUtils.isBlank(labformulier.getKlachtenAndersNamelijkTekst()))
		{
			addQuestionIssue(CodeSystem.KLACHTEN_VRIJE_TEKST);
		}
		else if (!labformulier.isKlachtenAndersNamelijk() &&
			!StringUtils.isBlank(labformulier.getKlachtenAndersNamelijkTekst()))
		{
			addQuestionIssue(CodeSystem.KLACHTEN_VRIJE_TEKST);
		}
	}

	private void validateDatumUitstrijkjeContent(CervixLabformulier labformulier)
	{
		if (labformulier.getDatumUitstrijkje() == null)
		{
			addQuestionIssue(CodeSystem.DATUM_UITSTRIJKJE);
		}
		else if (currentDateSupplier.getLocalDate()
			.isBefore(DateUtil.toLocalDate(labformulier.getDatumUitstrijkje())))
		{
			addQuestionIssue(CodeSystem.DATUM_UITSTRIJKJE);
		}
	}

	private void validateFormMapping(LabaanvraagBundle bundle,
		CervixLabformulier labformulier)
	{
		validateDatumMapping(bundle.getDatumUitstrijkje(), labformulier.getDatumUitstrijkje(), CodeSystem.DATUM_UITSTRIJKJE);
		validateKlachtenMapping(bundle, labformulier);
		validateMenstruatieMapping(bundle, labformulier);
		validateAnticonceptieMapping(bundle, labformulier);
		validateHormonenMapping(bundle, labformulier);
		validateAspectCervixMapping(bundle, labformulier);
		validateTekstMapping(labformulier.getOpmerkingenTekst(), bundle.getOpmerkingenTekst(), CodeSystem.OPMERKINGEN_TEKST);
	}

	private void validateKlachtenMapping(LabaanvraagBundle bundle, CervixLabformulier labformulier)
	{
		if (bundle.correctlyMappedList(CodeSystem.KLACHTEN))
		{
			validateListMapping(getNumberOfMappedKlachten(labformulier), bundle.getKlachten(), CodeSystem.KLACHTEN);
			validateTekstMapping(labformulier.getKlachtenAndersNamelijkTekst(), bundle.getKlachtenVrijeTekst(), CodeSystem.KLACHTEN_VRIJE_TEKST);
		}
		else
		{
			addQuestionIssue(CodeSystem.KLACHTEN);
		}
	}

	private void validateHormonenMapping(LabaanvraagBundle bundle, CervixLabformulier labformulier)
	{
		validateListMapping(getNumberOfMappedHormonenGebruik(labformulier), bundle.getGebruikHormonen(), CodeSystem.GEBRUIK_HORMONEN);
		validateTekstMapping(labformulier.getGebruikHormonenJaVanwegeTekst(), bundle.getGebruikHormonenVrijeTekst(), CodeSystem.GEBRUIK_HORMONEN_VRIJE_TEKST);
	}

	private void validateAspectCervixMapping(LabaanvraagBundle bundle, CervixLabformulier labformulier)
	{
		if (StringUtils.isNotBlank(bundle.getAspectCervix())
			&& !labformulier.isAspectCervixNormaal()
			&& !labformulier.isAspectCervixAbnormaalOfVerdachtePortio()
			&& !labformulier.isAspectCervixNietGezien())
		{
			addQuestionIssue(CodeSystem.ASPECT_CERVIX);
		}

		validateTekstMapping(labformulier.getAspectCervixAbnormaalOfVerdachtePortioTekst(), bundle.getAspectCervixVrijeTekst(), CodeSystem.ASPECT_CERVIX_VRIJE_TEKST);
	}

	private void validateTekstMapping(String inputTekst, String mappedTekst, CodeSystem codeSystem)
	{
		if (inputTekst != null)
		{
			if (!inputTekst.equalsIgnoreCase(mappedTekst.trim()))
			{
				addQuestionIssue(codeSystem);
			}
			else if (inputTekst.length() > 240)
			{
				addQuestionIssue(codeSystem, ": de maximum lengte is 240 karakters");
			}
		}
	}

	private void validateListMapping(long numberOfMappedItems, List<String> inputList, CodeSystem codeSystem)
	{
		if (numberOfMappedItems != inputList.size())
		{
			addQuestionIssue(codeSystem);
		}
	}

	private void validateAnticonceptieMapping(LabaanvraagBundle bundle, CervixLabformulier labformulier)
	{
		if (StringUtils.isNotBlank(bundle.getAnticonceptie())
			&& !labformulier.isAnticonceptieGeen()
			&& !labformulier.isAnticonceptiePil()
			&& !labformulier.isAnticonceptieIudKoper()
			&& !labformulier.isAnticonceptieIudMirena()
			&& !labformulier.isAnticonceptieAnders())
		{
			addQuestionIssue(CodeSystem.ANTICONCEPTIE);
		}
	}

	private void validateDatumMapping(String inputDate, Date mappedDate, CodeSystem codeSystem)
	{
		if (StringUtils.isNotBlank(inputDate)
			&& mappedDate == null)
		{
			addQuestionIssue(codeSystem);
		}
	}

	private void validateMenstruatieMapping(LabaanvraagBundle bundle, CervixLabformulier labformulier)
	{
		if (StringUtils.isNotBlank(bundle.getMenstruatie())
			&& !labformulier.isMenstruatieNormaal()
			&& !labformulier.isMenstruatieGeenMenstruatie()
			&& !labformulier.isMenstruatieMenopauze()
			&& !labformulier.isMenstruatiePostmenopauze())
		{
			addQuestionIssue(CodeSystem.MENSTRUATIE);
		}

		validateDatumMapping(bundle.getDatumLaatsteMenstruatie(), labformulier.getDatumLaatsteMenstruatie(), CodeSystem.DATUM_LAATSTE_MENSTRUATIE);
	}

	private long getNumberOfMappedKlachten(CervixLabformulier labformulier)
	{
		return Stream.of(
				labformulier.isKlachtenContactbloedingen(),
				labformulier.isKlachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak(),
				labformulier.isKlachtenIntermenstrueelBloedverlies(),
				labformulier.isKlachtenPostmenopauzaalBloedverlies(),
				labformulier.isKlachtenAndersNamelijk())
			.filter(Boolean.TRUE::equals)
			.count();
	}

	private long getNumberOfMappedHormonenGebruik(CervixLabformulier labformulier)
	{
		return Stream.of(
				labformulier.isGebruikHormonenJaVanwegeBorstkanker(),
				labformulier.isGebruikHormonenJaVanwegeOvergangsklachten(),
				labformulier.isGebruikHormonenJaVanwege())
			.filter(Boolean.TRUE::equals)
			.count();
	}

	private void validateClientGerelateerdeData(LabaanvraagResource resource)
	{
		var clientBsn = resource.getClientBsn();
		if (clientBsn == null)
		{
			addClientMissingIssues();
		}
		else if (BsnUtils.isValidBSN(clientBsn))
		{
			var client = clientService.getClientByBsn(clientBsn);
			if (client == null)
			{
				addClientNotFoundIssues();
			}
			else
			{
				validateMonsterGerelateerdeData(resource);
			}
		}
		else
		{
			addClientNotFoundIssues();
		}
	}

	private void addClientMissingIssues()
	{
		addErrorOperationOutcomeIssueComponent(OperationOutcome.IssueType.EXCEPTION,
			new RequiredPropertyException("Er dient een BSN voor de client meegezonden te worden."));
	}

	private void addMappingIssue(Exception e)
	{
		LOG.error("Exceptie in mapping digitaal labformulier: ", e);
		addErrorOperationOutcomeIssueComponent(OperationOutcome.IssueType.EXCEPTION,
			new RequiredPropertyException("Er heeft zich een onbekende fout voorgedaan tijdens de mapping van de Bundle."));
	}

	private void addMappingIssue(String detailMessage)
	{
		addErrorOperationOutcomeIssueComponent(OperationOutcome.IssueType.EXCEPTION,
			new RequiredPropertyException(String.format("De volgende fout is opgetreden: %s", detailMessage)));
	}

	private void addQuestionIssue(CodeSystem question)
	{
		addQuestionIssue(question, "");
	}

	private void addQuestionIssue(CodeSystem question, String extraMessage)
	{
		addErrorOperationOutcomeIssueComponent(OperationOutcome.IssueType.EXCEPTION,
			new RequiredPropertyException(String.format("Er heeft zich een probleem voorgedaan met de vraag %s [%s]%s", question.name(), question.getCode(), extraMessage)));
	}

	private void addClientNotFoundIssues()
	{
		addBusinessRuleErrorOperationOutcomeIssueComponent(ValidationMessage.BSN_ONBEKEND);
	}

	private void validateScreeningRonde(CervixUitstrijkje uitstrijkje, String bsn)
	{
		uitstrijkje = (CervixUitstrijkje) Hibernate.unproxy(uitstrijkje);
		var ontvangstRonde = uitstrijkje.getOntvangstScreeningRonde();
		var laatsteScreeningRonde = screeningRondeService.getLaatsteScreeningRonde(bsn);
		if (ontvangstRonde == null && laatsteScreeningRonde.isPresent())
		{
			uitstrijkje.setOntvangstScreeningRonde(laatsteScreeningRonde.get());
		}

		if (!screeningRondeService.heeftValideScreeningRondeVoorDigitaalLabformulier(uitstrijkje))
		{
			addBusinessRuleErrorOperationOutcomeIssueComponent(ValidationMessage.GEEN_GELDIGE_RONDE);
		}

		uitstrijkje.setOntvangstScreeningRonde(ontvangstRonde);
		hibernateSerice.saveOrUpdate(uitstrijkje);
	}

	private void validateMonsterGerelateerdeData(LabaanvraagResource resource)
	{
		if (resource.getMonsterId() != null)
		{
			getAndValidateMonsterByMonsterId(resource);
		}
		else if (resource.getControleLetters() != null)
		{
			getAndValidateMonsterByControleLetters(resource);
		}
		else
		{
			addNeitherMonsterIdNorControleLettersFoundOutcome();
		}
	}

	private void getAndValidateMonsterByMonsterId(LabaanvraagResource resource)
	{
		var uitstrijkje = monsterService
			.getUitstrijkjeByClientBsnAndMonsterId(resource.getClientBsn(), resource.getMonsterId()).orElse(null);
		if (uitstrijkje == null)
		{
			addBusinessRuleErrorOperationOutcomeIssueComponent(ValidationMessage.MONSTER_ID_ONBEKEND_BIJ_CLIENT);
		}
		else if (resource.getControleLetters() != null
			&& !resource.getControleLetters().equals(uitstrijkje.getControleLetters()))
		{
			addBusinessRuleErrorOperationOutcomeIssueComponent(ValidationMessage.COMBI_MONSTER_ID_CONTROLELETTERS_ONBEKEND);
		}
		else if (isMonsterReedsGebruikt(uitstrijkje))
		{
			addBusinessRuleErrorOperationOutcomeIssueComponent(ValidationMessage.MONSTER_REEDS_GEBRUIKT);
		}
		else
		{
			validateScreeningRonde(uitstrijkje, resource.getClientBsn());
		}
	}

	private boolean isMonsterReedsGebruikt(CervixUitstrijkje uitstrijkje)
	{
		return uitstrijkje.getLabformulier() != null;
	}

	private void getAndValidateMonsterByControleLetters(LabaanvraagResource resource)
	{
		var uitstrijkje = monsterService
			.getUitstrijkjeByClientBsnAndControleLetters(resource.getClientBsn(), resource.getControleLetters()).orElse(null);
		if (uitstrijkje == null)
		{
			addBusinessRuleErrorOperationOutcomeIssueComponent(ValidationMessage.CONTROLELETTERS_ONBEKEND_BIJ_CLIENT);
		}
		else if (isMonsterReedsGebruikt(uitstrijkje))
		{
			addBusinessRuleErrorOperationOutcomeIssueComponent(ValidationMessage.MONSTER_REEDS_GEBRUIKT);
		}
		else
		{
			validateScreeningRonde(uitstrijkje, resource.getClientBsn());
		}
	}

	private void addNeitherMonsterIdNorControleLettersFoundOutcome()
	{
		addErrorOperationOutcomeIssueComponent(OperationOutcome.IssueType.EXCEPTION,
			new RequiredPropertyException("Monster id & Controleletters ontbreken. 1 van de 2 dient tenminste meegezonden te worden."));
	}

	private void validateHuisartsGerelateerdeData(LabaanvraagResource resource)
	{
		if (resource instanceof LabaanvraagBundle)
		{
			var bundle = (LabaanvraagBundle) resource;
			if (bundle.getAuthorReferences().size() > 1)
			{
				addMappingIssue("meer dan één author reference");
				return;
			}
		}
		if (resource.getIndividueleAgb() != null)
		{
			validateIndividueleAgb(resource);
		}
		else if (resource.getPraktijkAgb() != null)
		{
			validatePraktijkAgb(resource);
		}
		else
		{
			addAgbMissingOutcome();
		}
	}

	private void validateIndividueleAgb(LabaanvraagResource resource)
	{
		var huisarts = huisartsLocatieService.findActieveHuisartsMetEenActieveLocatie(resource.getIndividueleAgb());
		if (huisarts.isEmpty() && resource.getPraktijkAgb() != null)
		{
			validatePraktijkAgb(resource);
		}
		else if (huisarts.isPresent())
		{
			validateActieveGoedIngevuldeLocatie(huisarts.get());
			validateGemeenteGekoppeldAanBmhkLaboratorium(resource);
		}
	}

	private void validatePraktijkAgb(LabaanvraagResource resource)
	{
		var huisarts = huisartsLocatieService.findActieveHuisartsMetEenActieveLocatie(resource.getPraktijkAgb());
		if (huisarts.isEmpty())
		{
			addBusinessRuleErrorOperationOutcomeIssueComponent(ValidationMessage.AGB_CODE_ONBEKEND);
		}
		else
		{
			validateActieveGoedIngevuldeLocatie(huisarts.get());
			validateGemeenteGekoppeldAanBmhkLaboratorium(resource);
		}
	}

	private void validateActieveGoedIngevuldeLocatie(CervixHuisarts huisarts)
	{
		var huisartsHeeftGeenGoedIngevuldeActieveLocatie = huisarts
			.getHuisartsLocaties()
			.stream()
			.noneMatch(locatie -> locatie.getStatus() == CervixLocatieStatus.ACTIEF && CervixLocatieUtil.isLocatieCompleet(locatie));

		if (huisartsHeeftGeenGoedIngevuldeActieveLocatie)
		{
			addErrorOperationOutcomeIssueComponent(OperationOutcome.IssueType.EXCEPTION,
				new RequiredPropertyException(String.format("Voor huisarts met id %s is geen actieve en/of goed ingevulde locatie gevonden.", huisarts.getId())));
		}
	}

	private void validateGemeenteGekoppeldAanBmhkLaboratorium(LabaanvraagResource resource)
	{
		if (resource instanceof LabaanvraagBundle)
		{
			var bundle = (LabaanvraagBundle) resource;
			try
			{
				if (bundle.getLaboratorium() == null)
				{
					addErrorOperationOutcomeIssueComponent(OperationOutcome.IssueType.EXCEPTION,
						new RequiredPropertyException("De gemeente waar de huisarts is gevestigd is (nog) niet aan een laboratorium gekoppeld. Neem contact op met BVO-NL!"));
				}
			}
			catch (NoSuchElementException e)
			{
				LOG.error(e.getMessage());
			}
		}
	}

	private void addAgbMissingOutcome()
	{
		addErrorOperationOutcomeIssueComponent(OperationOutcome.IssueType.EXCEPTION,
			new RequiredPropertyException("Er dient een AGB code voor de huisarts meegezonden te worden."));
	}

	private CodeableConcept createDetails(ValidationMessage message)
	{
		return new CodeableConcept()
			.setText(message.name());
	}

	public void addBusinessRuleErrorOperationOutcomeIssueComponent(ValidationMessage message)
	{
		addOperationOutcomeIssueComponent(OperationOutcome.IssueSeverity.ERROR, OperationOutcome.IssueType.BUSINESSRULE, message);
	}

	private void addErrorOperationOutcomeIssueComponent(OperationOutcome.IssueType issueType,
		Exception exception)
	{
		addOperationOutcomeIssueComponent(OperationOutcome.IssueSeverity.ERROR, issueType, exception);
	}

	private void addOperationOutcomeIssueComponent(OperationOutcome.IssueSeverity severity,
		OperationOutcome.IssueType issueType,
		ValidationMessage message)
	{
		LOG.error("Validatiefout digitaal labformulier: {}", message);
		addOperationOutcomeIssueComponent(new OperationOutcome.OperationOutcomeIssueComponent()
			.setSeverity(severity)
			.setCode(issueType)
			.setDetails(createDetails(message)));
	}

	private void addOperationOutcomeIssueComponent(OperationOutcome.IssueSeverity severity,
		OperationOutcome.IssueType issueType,
		Exception exception)
	{
		LOG.error("Validatiefout digitaal labformulier: {} ", exception.getMessage());
		addOperationOutcomeIssueComponent(new OperationOutcome.OperationOutcomeIssueComponent()
			.setSeverity(severity)
			.setCode(issueType)
			.setDiagnosticsElement(new StringType(exception.getClass().getSimpleName() + ": " + exception.getMessage())));
	}

	private void addOperationOutcomeIssueComponent(OperationOutcome.OperationOutcomeIssueComponent outcomeIssueComponent)
	{
		if (operationOutcome.getIssue().stream().noneMatch(i -> i.equalsDeep(outcomeIssueComponent)))
		{
			operationOutcome.addIssue(outcomeIssueComponent);
		}
	}

	public boolean isSuccesvol()
	{
		if (validated)
		{
			return operationOutcome.isEmpty();
		}
		throw new NotValidatedException("Validatie succes kan nog niet worden bepaald, daarvoor moet er eerst worden gevalideerd");
	}

	public OperationOutcome toOperationOutcome()
	{
		return operationOutcome;
	}

}
