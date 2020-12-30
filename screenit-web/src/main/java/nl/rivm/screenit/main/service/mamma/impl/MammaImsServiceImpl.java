package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.MammaImsErrorType;
import nl.rivm.screenit.main.service.mamma.MammaImsService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.rivm.screenit.model.mamma.imsapi.FhirCodableConcept;
import nl.rivm.screenit.model.mamma.imsapi.FhirCoding;
import nl.rivm.screenit.model.mamma.imsapi.FhirContext;
import nl.rivm.screenit.model.mamma.imsapi.FhirFocus;
import nl.rivm.screenit.model.mamma.imsapi.FhirIdentifiableEntity;
import nl.rivm.screenit.model.mamma.imsapi.FhirIdentifier;
import nl.rivm.screenit.model.mamma.imsapi.FhirImagingStudy;
import nl.rivm.screenit.model.mamma.imsapi.FhirPatientStudy;
import nl.rivm.screenit.model.mamma.imsapi.FhirUserSession;
import nl.rivm.screenit.model.mamma.imsapi.FhirWorklistItem;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.functionalinterfaces.StringResolver;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true) 
public class MammaImsServiceImpl implements MammaImsService
{
	@Autowired
	private HibernateService hibernateService;

	private static ObjectMapper mapper = new ObjectMapper();

	@Autowired
	private LogService logService;

	@Override
	public String createDesktopSyncMessage(Gebruiker gebruiker, MammobridgeRole role, Long huidigeOnderzoekId, List<Long> komendeBeoordelingIds)
	{
		FhirUserSession userSession = createUserSession(gebruiker, role);

		MammaOnderzoek huidigeOnderzoek = hibernateService.get(MammaOnderzoek.class, huidigeOnderzoekId);

		userSession.setFocus(createFocus(huidigeOnderzoek));

		List<FhirWorklistItem> worklist = createWorklist(komendeBeoordelingIds);

		userSession.setContext(createUpcomingCasesContext(worklist));

		return toJsonString(userSession);
	}

	@Override
	public String createEmptyDesktopSyncMessage(Gebruiker gebruiker, MammobridgeRole role)
	{
		FhirUserSession userSession = createUserSession(gebruiker, role);
		userSession.setFocus(createEmptyFocus());
		userSession.setContext(createUpcomingCasesContext(Collections.emptyList()));
		return toJsonString(userSession);
	}

	@Override
	public String createAllImagesSeenMessage(Gebruiker gebruiker, MammobridgeRole role, MammaOnderzoek onderzoek)
	{
		FhirUserSession userSession = createUserSession(gebruiker, role);
		FhirFocus focus = createFocus(onderzoek);
		FhirContext context = createContext("layoutImages", "requestLayoutsImagesSeenCurrentFocus");
		userSession.setFocus(focus);
		userSession.setContext(context);
		return toJsonString(userSession);
	}

	@Override
	public FhirUserSession parseFhirMessage(String json) throws IOException
	{
		return mapper.readValue(json, FhirUserSession.class);
	}

	@Override
	public String createLogonMessage(Gebruiker gebruiker, MammobridgeRole role)
	{
		FhirUserSession userSession = createUserSession(gebruiker, role);
		userSession.setContext(createLogonContext());

		return toJsonString(userSession);
	}

	@Override
	public String createLogoffMessage(Gebruiker gebruiker, MammobridgeRole role)
	{
		FhirUserSession userSession = createUserSession(gebruiker, role);
		userSession.setContext(createLogoffContext());

		return toJsonString(userSession);
	}

	private FhirUserSession createUserSession(Gebruiker gebruiker, MammobridgeRole role)
	{
		FhirUserSession userSession = new FhirUserSession();
		userSession.setUser(createUser(gebruiker, role));
		return userSession;
	}

	private FhirFocus createFocus(MammaOnderzoek onderzoek)
	{
		FhirFocus focus = new FhirFocus();
		setPatientStudy(focus, onderzoek);
		return focus;
	}

	private FhirFocus createEmptyFocus()
	{
		FhirFocus focus = new FhirFocus();
		focus.setPatient(createPatient(""));
		focus.setImagingStudy(createStudy(""));
		return focus;
	}

	private List<FhirWorklistItem> createWorklist(List<Long> komendeBeoordelingIds)
	{
		List<FhirWorklistItem> worklist = new ArrayList<>();
		for (Long beoordelingId : komendeBeoordelingIds)
		{
			worklist.add(createWorklistItem(beoordelingId));
		}
		return worklist;
	}

	private FhirWorklistItem createWorklistItem(Long beoordelingId)
	{
		MammaBeoordeling beoordeling = hibernateService.get(MammaBeoordeling.class, beoordelingId);
		FhirWorklistItem worklistItem = new FhirWorklistItem();
		setPatientStudy(worklistItem, beoordeling.getOnderzoek());
		return worklistItem;
	}

	private void setPatientStudy(FhirPatientStudy patientStudy, MammaOnderzoek onderzoek)
	{
		patientStudy.setPatient(createPatient(getBsn(onderzoek)));
		patientStudy.setImagingStudy(createStudy(getAccessionNumber(onderzoek)));
	}

	private String getBsn(MammaOnderzoek onderzoek)
	{
		return getClient(onderzoek).getPersoon().getBsn();
	}

	private Client getClient(MammaOnderzoek onderzoek)
	{
		return getScreeningRonde(onderzoek).getDossier().getClient();
	}

	private MammaScreeningRonde getScreeningRonde(MammaOnderzoek onderzoek)
	{
		return onderzoek.getAfspraak().getUitnodiging().getScreeningRonde();
	}

	private String getAccessionNumber(MammaOnderzoek onderzoek)
	{
		return Long.toString(getScreeningRonde(onderzoek).getUitnodigingsNr());
	}

	private FhirIdentifiableEntity createUser(Gebruiker username, MammobridgeRole role)
	{
		FhirIdentifiableEntity user = new FhirIdentifiableEntity();
		user.setIdentifier(createIndentifier("ScreenIT", username.getGebruikersnaam()));
		user.getIdentifier().setType(createRole(role));
		return user;
	}

	private FhirCodableConcept createRole(MammobridgeRole role)
	{
		FhirCodableConcept type = new FhirCodableConcept();
		type.setCoding(new FhirCoding());
		type.getCoding().setCode(role.getIds7Role());
		type.getCoding().setSystem("ScreenIT");
		return type;
	}

	private FhirIdentifiableEntity createPatient(String bsn)
	{
		FhirIdentifiableEntity patient = new FhirIdentifiableEntity();
		patient.setIdentifier(createIndentifier("NLMINBIZA", bsn));
		return patient;
	}

	private FhirImagingStudy createStudy(String accessionNumber)
	{
		FhirImagingStudy study = new FhirImagingStudy();
		study.setAccession(createIndentifier("ScreenIT", accessionNumber));
		study.setIdentifier(createIndentifier("ScreenIT", accessionNumber));
		return study;
	}

	private FhirContext createUpcomingCasesContext(List<FhirWorklistItem> worklist)
	{
		FhirContext context = createContext("Worklist", "UpcomingCases");
		context.setWorklist(worklist);
		return context;
	}

	private FhirContext createLogonContext()
	{
		return createContext("Session", "LogOn");
	}

	private FhirContext createLogoffContext()
	{
		return createContext("Session", "LogOff");
	}

	private FhirContext createContext(String type, String value)
	{
		FhirContext context = new FhirContext();
		context.setType(type);
		context.setValue(value);
		return context;
	}

	private FhirIdentifier createIndentifier(String system, String value)
	{
		FhirIdentifier identifier = new FhirIdentifier();
		identifier.setSystem(system);
		identifier.setValue(value);
		return identifier;
	}

	private String toJsonString(FhirUserSession userSession)
	{
		try
		{
			ObjectMapper objectMapper = new ObjectMapper();
			return objectMapper.writeValueAsString(userSession);
		}
		catch (JsonProcessingException e)
		{
			throw new IllegalStateException("Cannot convert UserSession to Json", e);
		}
	}

	@Override
	public String handleError(String error, InstellingGebruiker gebruiker, StringResolver stringResolver, Long onderzoekId)
	{
		MammaImsErrorType errorType = MammaImsErrorType.findForCode(error);
		String foutmelding = stringResolver.resolveString(errorType.getMeldingProperty());
		logService.logGebeurtenis(errorType.getLogGebeurtenis(), new LogEvent(foutmelding + " (" + error + ")"), gebruiker, getClientVoorLogGebeurtenis(onderzoekId));
		return foutmelding;
	}

	private Client getClientVoorLogGebeurtenis(Long onderzoekId)
	{
		return onderzoekId != null ? getClient(hibernateService.get(MammaOnderzoek.class, onderzoekId)) : null;
	}
}
