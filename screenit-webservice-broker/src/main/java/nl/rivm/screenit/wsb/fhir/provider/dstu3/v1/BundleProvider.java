package nl.rivm.screenit.wsb.fhir.provider.dstu3.v1;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixLabformulierService;
import nl.rivm.screenit.wsb.fhir.interceptor.FQDNProvider;
import nl.rivm.screenit.wsb.fhir.mapper.LabaanvraagMapper;
import nl.rivm.screenit.wsb.fhir.resource.dstu3.v1.LabaanvraagBundle;
import nl.rivm.screenit.wsb.fhir.validator.LabaanvraagValidator;
import nl.rivm.screenit.wsb.fhir.validator.ValidationMessage;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.apache.commons.lang.StringUtils;
import org.hibernate.exception.GenericJDBCException;
import org.hl7.fhir.dstu3.model.CodeableConcept;
import org.hl7.fhir.dstu3.model.OperationOutcome;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.orm.hibernate5.HibernateJdbcException;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.annotation.Create;
import ca.uhn.fhir.rest.annotation.ResourceParam;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.server.exceptions.UnprocessableEntityException;
import ca.uhn.fhir.validation.SingleValidationMessage;
import ca.uhn.fhir.validation.ValidationResult;

public class BundleProvider extends BaseResourceProvider
{
	private static final Logger LOG = LoggerFactory.getLogger(BundleProvider.class);

	private final ClientService clientService;

	private final HibernateService hibernateService;

	private final LogService logService;

	private final DashboardService dashboardService;

	private final CervixLabformulierService labformulierService;

	private final FQDNProvider fqdnProvider;

	public BundleProvider()
	{
		clientService = ApplicationContextProvider.getApplicationContext().getBean(ClientService.class);
		hibernateService = ApplicationContextProvider.getApplicationContext().getBean(HibernateService.class);
		logService = ApplicationContextProvider.getApplicationContext().getBean(LogService.class);
		dashboardService = ApplicationContextProvider.getApplicationContext().getBean(DashboardService.class);
		labformulierService = ApplicationContextProvider.getApplicationContext().getBean(CervixLabformulierService.class);
		fqdnProvider = ApplicationContextProvider.getApplicationContext().getBean(FQDNProvider.class);
	}

	@Override
	public Class<? extends IBaseResource> getResourceType()
	{
		return LabaanvraagBundle.class;
	}

	@Create()
	public MethodOutcome create(@ResourceParam LabaanvraagBundle bundle)
	{
		try
		{
			FhirContext context = FhirContext.forDstu3();
			ValidationResult result = getFhirValidator(context)
				.validateWithResult(bundle);

			if (result.isSuccessful()) 
			{
				LabaanvraagValidator labAanvraagValidator = new LabaanvraagValidator();
				if (labAanvraagValidator.validate(bundle).isSuccesvol()) 
				{
					try
					{
						doAanvraag(bundle);
						logGebeurtenis(bundle, HttpStatus.CREATED, Collections.emptyList());
						return new MethodOutcome().setOperationOutcome(result.toOperationOutcome());
					}
					catch (GenericJDBCException | HibernateJdbcException e)
					{
						updateLabaanvraagValidatorBijDubbelMonster(labAanvraagValidator, e);
					}
				}
				logGebeurtenis(bundle, HttpStatus.UNPROCESSABLE_ENTITY, labAanvraagValidator.toOperationOutcome().getIssue());
				throw new UnprocessableEntityException(context, labAanvraagValidator.toOperationOutcome());
			}
			logGebeurtenis(bundle, HttpStatus.UNPROCESSABLE_ENTITY, result.getMessages());
			throw new UnprocessableEntityException(context, result.toOperationOutcome());
		}
		catch (UnprocessableEntityException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			String melding = String.format("Er is een onbekende fout opgetreden. Neem contact op met de helpdesk. (FQDN:%s)", fqdnProvider.getCurrentFQDN());
			logGebeurtenis(Level.WARNING, melding, melding, null);
			LOG.error("Er is een onbekende fout opgetreden.", e);
			throw new UnprocessableEntityException("Er is een onbekende fout opgetreden. Neem contact op met " + Constants.NAAM_BEHEER_ORGANISATIE + ".", e);
		}
	}

	private void updateLabaanvraagValidatorBijDubbelMonster(LabaanvraagValidator labAanvraagValidator, RuntimeException e)
	{
		Throwable cause = e.getCause();
		while (cause != null)
		{
			if (cause.getMessage().contains("Monster reeds gebruikt"))
			{
				labAanvraagValidator.addBusinessRuleErrorOperationOutcomeIssueComponent(ValidationMessage.MONSTER_REEDS_GEBRUIKT);
				return;
			}
			cause = cause.getCause();
		}
		throw e;
	}

	private void doAanvraag(LabaanvraagBundle bundle) throws GenericJDBCException, HibernateJdbcException
	{
		CervixLabformulier labformulier = LabaanvraagMapper.INSTANCE.toLabformulier(bundle);
		labformulier.setLeverancierFqdn(fqdnProvider.getCurrentFQDN());
		hibernateService.saveOrUpdate(labformulier);
		labformulierService.koppelDigitaalLabformulier(labformulier);
	}

	private void logGebeurtenis(LabaanvraagBundle bundle, HttpStatus httpStatus, List<?> valiationOutcomes)
	{
		String melding = getMelding(bundle, httpStatus, valiationOutcomes);
		String gegevensMeldingPart = getGegevensMeldingPart(bundle);
		Client clientByBsn = clientService.getClientByBsn(bundle.getClientBsn());

		logGebeurtenis(httpStatus.value() == 201 ? Level.INFO : Level.WARNING, melding, gegevensMeldingPart, clientByBsn);
	}

	private void logGebeurtenis(Level level, String logMelding, String oudLogFinderTekst, Client client)
	{
		String bsn = null;

		if (client != null)
		{
			bsn = client.getPersoon().getBsn();
		}
		var foutLogGebeurtenis = LogGebeurtenis.CERVIX_DIGITAAL_LABFORMULIER_FOUT_ONTVANGEN;
		boolean heeftBestaandeWarningLogregelInAfgelopenDag = level == Level.WARNING && !logService.heeftGeenBestaandeLogregelBinnenPeriode(List.of(foutLogGebeurtenis), bsn, null,
			logMelding, 1);
		if (!heeftBestaandeWarningLogregelInAfgelopenDag)
		{
			if (bsn != null)
			{
				dashboardService.verwijderLogRegelsVanDashboards(List.of(foutLogGebeurtenis), bsn, null);
			}
			dashboardService.verwijderLogRegelsVanDashboards(List.of(foutLogGebeurtenis), null, oudLogFinderTekst);
			logService.logGebeurtenis(level == Level.WARNING ? foutLogGebeurtenis : LogGebeurtenis.CERVIX_DIGITAAL_LABFORMULIER_ONTVANGEN, client, logMelding,
				Bevolkingsonderzoek.CERVIX);
		}
	}

	private String getMelding(LabaanvraagBundle bundle, HttpStatus httpStatus, List<?> valiationOutcomes)
	{
		return String.format("Validatie %ssuccesvol:%s %s",
			httpStatus.getReasonPhrase().equals("Created")
				? ""
				: "on",
			httpStatus.value() == 201
				? ""
				: String.format(" fout(en)[%s],", getValidationErrors(valiationOutcomes)),
			getGegevensMeldingPart(bundle));
	}

	private String getGegevensMeldingPart(LabaanvraagBundle bundle)
	{
		var zorgDomeinID = bundle.getZorgDomeinID();
		var zorgDomeinLogString = StringUtils.isNotBlank(zorgDomeinID) ? String.format(", ZorgDomeinID: %s", zorgDomeinID) : "";

		return String.format("gegevens[BSN: %s, AGB individueel: %s, AGB praktijk: %s, Monster-id: %s, Controleletters: %s%s]. (FQDN:%s)",
			bundle.getClientBsn(),
			bundle.getIndividueleAgb(),
			bundle.getPraktijkAgb(),
			bundle.getMonsterId(),
			bundle.getControleLetters(),
			zorgDomeinLogString,
			fqdnProvider.getCurrentFQDN());
	}

	private String getValidationErrors(List<?> list)
	{
		return !list.isEmpty() && list.get(0) instanceof SingleValidationMessage
			? getSingleValidationMessages(((List<SingleValidationMessage>) list))
			: getOperationOutcomeValidationMessages(((List<OperationOutcome.OperationOutcomeIssueComponent>) list));
	}

	private String getSingleValidationMessages(List<SingleValidationMessage> list)
	{
		if (!list.isEmpty())
		{
			return list.stream()
				.map(SingleValidationMessage::getMessage)
				.collect(Collectors.joining(","));
		}
		return "";
	}

	private String getOperationOutcomeValidationMessages(List<OperationOutcome.OperationOutcomeIssueComponent> list)
	{
		if (!list.isEmpty())
		{
			return Stream.concat(
					list.stream()
						.map(OperationOutcome.OperationOutcomeIssueComponent::getDiagnostics),
					list.stream()
						.map(OperationOutcome.OperationOutcomeIssueComponent::getDetails)
						.filter(Objects::nonNull)
						.map(CodeableConcept::getText))
				.filter(Objects::nonNull)
				.collect(Collectors.joining(", "));
		}
		return "";
	}

}
