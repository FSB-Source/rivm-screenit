package nl.rivm.screenit.wsb.vrld;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jws.WebMethod;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.formulieren.IdentifierElement;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectVragenlijst;
import nl.rivm.screenit.service.VragenlijstBaseService;
import nl.rivm.screenit.ws.vrld.Response;
import nl.rivm.screenit.ws.vrld.Vraag;
import nl.rivm.screenit.ws.vrld.VragenlijstDefinitieService;
import nl.rivm.screenit.ws.vrld.VragenlijstProcessingException_Exception;
import nl.topicuszorg.formulieren2.api.definitie.AntwoordDefinitie;
import nl.topicuszorg.formulieren2.api.definitie.VraagDefinitie;
import nl.topicuszorg.formulieren2.api.instantie.FormulierActieInstantie;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElement;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElementContainer;
import nl.topicuszorg.formulieren2.api.instantie.VraagInstantie;
import nl.topicuszorg.formulieren2.persistence.definitie.DefaultAntwoordKeuzeVraagDefinitieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.VraagInstantieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.acties.StringShowVraagActieInstantie;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
@WebService(targetNamespace = "http://screenit.rivm.nl/", name = "VragenlijstDefinitieService")
@AllArgsConstructor
public class VragenlijstDefinitieServiceImpl implements VragenlijstDefinitieService
{

	private final VragenlijstBaseService vragenlijstBaseService;

	private final HibernateService hibernateService;

	private final Boolean testModus;

	@Override
	@WebResult(name = "return", targetNamespace = "")
	@RequestWrapper(localName = "getVragenlijstDefinitie", targetNamespace = "http://screenit.rivm.nl/", className = "nl.rivm.screenit.ws.vrld.GetVragenlijstDefinitie")
	@WebMethod(action = "{http://screenit.rivm.nl/}getVragenlijstDefinitie")
	@ResponseWrapper(localName = "getVragenlijstDefinitieResponse", targetNamespace = "http://screenit.rivm.nl/", className = "nl.rivm.screenit.ws.vrld.GetVragenlijstDefinitieResponse")
	@Transactional(propagation = Propagation.REQUIRED)
	public Response getVragenlijstDefinitie(String documentId) throws VragenlijstProcessingException_Exception
	{
		Response response = new Response();
		response.setCode(0);
		ScreenitFormulierInstantie formulierInstantie = null;
		if (StringUtils.isNumeric(documentId))
		{
			ProjectBrief projectBrief = hibernateService.get(ProjectBrief.class, Long.valueOf(documentId));
			if (projectBrief != null && projectBrief.getVragenlijstAntwoordenHolder() != null)
			{
				formulierInstantie = projectBrief.getVragenlijstAntwoordenHolder().getVragenlijstAntwoorden().getFormulierInstantie();
			}
			else
			{
				response.setCode(2);

			}
		}
		else
		{

			response.setCode(2);
		}
		if (formulierInstantie == null && Boolean.TRUE.equals(testModus) && "0".equals(documentId))
		{
			List<ProjectVragenlijst> vragenlijsten = vragenlijstBaseService.searchVragenlijsten(new ProjectVragenlijst(), -1, -1, "id", false);

			if (!vragenlijsten.isEmpty())
			{
				formulierInstantie = vragenlijsten.get(0).getFormulierInstantie();
			}
		}
		if (formulierInstantie != null)
		{
			processVragenlijst(formulierInstantie, response);
		}
		else if (response.getCode() == 0)
		{

			response.setCode(1);
		}

		return response;
	}

	private void processVragenlijst(ScreenitFormulierInstantie vragenlijst, Response response)
	{
		Map<String, StringShowVraagActieInstantie> acties = new HashMap<>();
		for (FormulierActieInstantie<?, ?> actieInstantie : vragenlijst.getActies())
		{
			StringShowVraagActieInstantie actie = (StringShowVraagActieInstantie) actieInstantie;
			VraagDefinitie<?> vraagDefinitie = ((VraagInstantieImpl<?>) actie.getTargetElement()).getVraagDefinitie();
			acties.put(((IdentifierElement) vraagDefinitie).getIdentifier(), actie);
		}
		processElementContainer(vragenlijst.getContainer(), acties, response);
	}

	private void processElementContainer(FormulierElementContainer<?> elementContainer, Map<String, StringShowVraagActieInstantie> acties, Response response)
	{
		for (FormulierElement formulierElement : elementContainer.getElementen())
		{
			if (formulierElement instanceof FormulierElementContainer)
			{
				processElementContainer((FormulierElementContainer<?>) formulierElement, acties, response);
			}
			else if (formulierElement instanceof VraagInstantie)
			{
				processVraag((VraagInstantie<?>) formulierElement, acties, response);
			}
		}
	}

	private void processVraag(VraagInstantie<?> vraagIntstantie, Map<String, StringShowVraagActieInstantie> acties, Response response)
	{
		@SuppressWarnings("unchecked")
		DefaultAntwoordKeuzeVraagDefinitieImpl<AntwoordDefinitie<?>> vraagDefinitie = (DefaultAntwoordKeuzeVraagDefinitieImpl<AntwoordDefinitie<?>>) vraagIntstantie
			.getVraagDefinitie();

		int nummer = response.getVragens().size() + 1;
		Vraag vraag = new Vraag();
		vraag.setNummer(nummer);
		String vraagId = ((IdentifierElement) vraagDefinitie).getIdentifier();

		vraag.setVerplicht(vraagDefinitie.getVerplichting() != null && acties.get(vraagId) == null);
		vraag.setMultiplechoice(vraagDefinitie.isMeervoudig());
		vraag.setTekst(vraagDefinitie.getVraag());
		vraag.setType("CHECKBOX_VERTICAL");

		for (AntwoordDefinitie<?> antwoordDefinitie : vraagDefinitie.getMogelijkeAntwoorden())
		{
			vraag.getAntwoordens().add(antwoordDefinitie.getAntwoordString());
		}
		response.getVragens().add(vraag);
	}

}
