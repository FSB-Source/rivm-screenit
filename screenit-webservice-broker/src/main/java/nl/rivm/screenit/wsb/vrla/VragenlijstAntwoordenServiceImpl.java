package nl.rivm.screenit.wsb.vrla;

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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.jws.WebMethod;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.formulieren.IdentifierElement;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectVragenlijstAntwoordenHolder;
import nl.rivm.screenit.model.project.ProjectVragenlijstStatus;
import nl.rivm.screenit.model.project.ScannedVragenlijst;
import nl.rivm.screenit.model.vragenlijsten.VragenlijstAntwoorden;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.ws.vrla.Response;
import nl.rivm.screenit.ws.vrla.Vraag;
import nl.rivm.screenit.ws.vrla.Vragenlijst;
import nl.rivm.screenit.ws.vrla.VragenlijstAntwoordenService;
import nl.rivm.screenit.ws.vrla.VragenlijstProcessingException_Exception;
import nl.topicuszorg.formulieren2.api.instantie.FormulierActieInstantie;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElement;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElementContainer;
import nl.topicuszorg.formulieren2.api.instantie.VraagInstantie;
import nl.topicuszorg.formulieren2.api.rendering.AntwoordRenderType;
import nl.topicuszorg.formulieren2.persistence.definitie.DefaultAntwoordKeuzeVraagDefinitieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.VraagInstantieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.acties.StringShowVraagActieInstantie;
import nl.topicuszorg.formulieren2.persistence.resultaat.AbstractAntwoord;
import nl.topicuszorg.formulieren2.persistence.resultaat.MeervoudigStringAntwoord;
import nl.topicuszorg.formulieren2.persistence.resultaat.StringAntwoord;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
@WebService(targetNamespace = "http://screenit.rivm.nl/", name = "VragenlijstAntwoordenService")
@Slf4j
@AllArgsConstructor
public class VragenlijstAntwoordenServiceImpl implements VragenlijstAntwoordenService
{
	private final HibernateService hibernateService;

	private final InstellingService instellingService;

	private final LogService logService;

	@Override
	@WebResult(name = "return", targetNamespace = "")
	@RequestWrapper(localName = "vragenlijstScanned", targetNamespace = "http://screenit.rivm.nl/", className = "nl.rivm.screenit.ws.vrla.VragenlijstScanned")
	@WebMethod
	@ResponseWrapper(localName = "vragenlijstScannedResponse", targetNamespace = "http://screenit.rivm.nl/", className = "nl.rivm.screenit.ws.vrla.VragenlijstScannedResponse")
	@Transactional(propagation = Propagation.REQUIRED)
	public Response vragenlijstScanned(Vragenlijst vragenlijst) throws VragenlijstProcessingException_Exception
	{
		LogGebeurtenis gebeurtenis = LogGebeurtenis.VRAGENLIJST_AFGEWEZEN;
		Client client = null;
		String melding = "Op papier ontvangen - Barcode: " + vragenlijst.getDocumentId() + " Status: " + vragenlijst.getStatus();

		Response response = new Response();
		response.setCode(1);
		response.setMisluktwerkbak("");

		try
		{
			if (StringUtils.isNumeric(vragenlijst.getDocumentId()))
			{
				ProjectBrief projectBrief = hibernateService.get(ProjectBrief.class, Long.valueOf(vragenlijst.getDocumentId()));
				ProjectVragenlijstAntwoordenHolder vragenlijstAntwoordenHolder = projectBrief.getVragenlijstAntwoordenHolder();
				if (projectBrief != null && vragenlijstAntwoordenHolder != null)
				{
					client = projectBrief.getClient();

					response.setMisluktwerkbak(projectBrief.getDefinitie().getMisluktBak());
					if (!vragenlijstAntwoordenHolder.getStatus().equals(ProjectVragenlijstStatus.AFGEROND))
					{
						VragenlijstProcessor vragenlijstProcessor = new VragenlijstProcessor(vragenlijst, projectBrief, response);
						Set<AbstractAntwoord<?>> antwoordSet = vragenlijstProcessor.run();
						if (ScannedVragenlijst.STATUS_AFGEHANDELD.equals(vragenlijst.getStatus()) && response.getFoutens().isEmpty()
							|| ScannedVragenlijst.STATUS_VERWIJDERD.equals(vragenlijst.getStatus()))
						{
							ScannedVragenlijst scannedVragenlijst = new ScannedVragenlijst();
							scannedVragenlijst.setBarcode(vragenlijst.getDocumentId());
							scannedVragenlijst.setObjid(vragenlijst.getObjId());
							scannedVragenlijst.setStatus(vragenlijst.getStatus());
							scannedVragenlijst.setScanDatum(vragenlijst.getScanDatum().toGregorianCalendar().getTime());
							scannedVragenlijst.setLabId(instellingService.getIfobtLabByLabID(vragenlijst.getLabId()));
							scannedVragenlijst.setFouten(new HashSet<String>(response.getFoutens()));

							vragenlijstAntwoordenHolder.getVragenlijstAntwoorden().getResultaat().setAntwoorden(antwoordSet);
							vragenlijstAntwoordenHolder.setScannedVragenlijst(scannedVragenlijst);
							vragenlijstAntwoordenHolder.setStatus(ProjectVragenlijstStatus.AFGEROND);

							hibernateService.saveOrUpdate(vragenlijstAntwoordenHolder);
							hibernateService.saveOrUpdate(vragenlijstAntwoordenHolder.getScannedVragenlijst());
							hibernateService.saveOrUpdate(vragenlijstAntwoordenHolder.getVragenlijstAntwoorden());
							hibernateService.saveOrUpdate(vragenlijstAntwoordenHolder.getVragenlijstAntwoorden().getResultaat());

							response.getFoutens().clear();
							response.setCode(0);
							gebeurtenis = LogGebeurtenis.VRAGENLIJST_AFGEROND;
						}
						else
						{
							if (ScannedVragenlijst.STATUS_ONBETROUWBAAR.equals(vragenlijst.getStatus()))
							{
								response.getFoutens().add("De scan heeft status onbetrouwbaar");
							}
							if (ScannedVragenlijst.STATUS_AFGEHANDELD.equals(vragenlijst.getStatus()) || ScannedVragenlijst.STATUS_ONBETROUWBAAR.equals(vragenlijst.getStatus()))
							{
								response.setCode(4);
							}
							melding += " - Fouten bij invullen vragenlijst";
						}
					}
					else
					{
						response.getFoutens().add("Al bestaande vragenlijst");
						response.setCode(3);
						melding += " - Al bestaande vragenlijst";
					}
				}
				else
				{
					response.getFoutens().add("Niet bestaande barcode");
					response.setCode(2);
					melding += " - Niet bestaande barcode";
				}
			}
			else
			{
				response.getFoutens().add("Niet bestaande barcode");
				response.setCode(2);
				melding += " - Niet bestaande barcode";
			}
		}
		catch (Exception e)
		{
			LOG.error("Onverwachtte fout", e);
			melding += " - Onverwachtte fout";
		}

		logService.logGebeurtenis(gebeurtenis, client, melding);
		return response;
	}

	private static class VragenlijstProcessor
	{
		private final Vragenlijst gegevenAntwoorden;

		private final ProjectBrief projectBrief;

		private final Response response;

		private final Map<Integer, Set<Integer>> gegevenAntwoordenMap = new HashMap<>();

		private final Map<String, Set<String>> teValiderenAntwoordenMap = new HashMap<>();

		private final Map<String, StringShowVraagActieInstantie> actieMap = new HashMap<>();

		private final Set<AbstractAntwoord<?>> resultaatAntwoorden;

		private int aantalVragenInDefinitie = 0;

		private VragenlijstProcessor(Vragenlijst gegevenAntwoorden, ProjectBrief projectBrief, Response response)
		{
			this.gegevenAntwoorden = gegevenAntwoorden;
			this.projectBrief = projectBrief;
			this.response = response;

			resultaatAntwoorden = new HashSet<>();

			maakAntwoordenMap();
			maakActieMap();
		}

		private Set<AbstractAntwoord<?>> run()
		{
			VragenlijstAntwoorden<ProjectVragenlijstAntwoordenHolder> vragenlijstAntwoorden = projectBrief.getVragenlijstAntwoordenHolder().getVragenlijstAntwoorden();
			processElementContainer((FormulierElementContainer) vragenlijstAntwoorden.getFormulierInstantie().getContainer());

			if (aantalVragenInDefinitie != gegevenAntwoorden.getVragens().size())
			{
				response.getFoutens().add(
					String.format("Er worden in totaal %d vragen verwacht maar er zijn %d vragen ontvangen.", aantalVragenInDefinitie, gegevenAntwoorden.getVragens().size()));
			}

			return resultaatAntwoorden;
		}

		private void maakAntwoordenMap()
		{
			for (Vraag vraag : gegevenAntwoorden.getVragens())
			{
				Integer identifier = Integer.valueOf(vraag.getIdentifier());
				Set<Integer> antwoorden = new HashSet<>();
				for (String antwoord : vraag.getAntwoordens())
				{
					antwoorden.add(Integer.valueOf(antwoord));
				}
				gegevenAntwoordenMap.put(identifier, antwoorden);
			}
		}

		private void maakActieMap()
		{
			VragenlijstAntwoorden<ProjectVragenlijstAntwoordenHolder> vragenlijstAntwoorden = projectBrief.getVragenlijstAntwoordenHolder().getVragenlijstAntwoorden();
			for (FormulierActieInstantie<?, ?> actieInstantie : vragenlijstAntwoorden.getFormulierInstantie().getActies())
			{
				StringShowVraagActieInstantie actie = (StringShowVraagActieInstantie) actieInstantie;
				VraagInstantie targetElement = (VraagInstantie) actie.getTargetElement();
				String identifier = ((IdentifierElement) targetElement.getVraagDefinitie()).getIdentifier();
				actieMap.put(identifier, actie);
			}
		}

		private void processElementContainer(FormulierElementContainer<FormulierElement> elementContainer)
		{
			for (FormulierElement formulierElement : elementContainer.getElementen())
			{
				if (formulierElement instanceof FormulierElementContainer)
				{
					processElementContainer((FormulierElementContainer) formulierElement);
				}
				else if (formulierElement instanceof VraagInstantieImpl)
				{
					processVraag((VraagInstantieImpl) formulierElement);
				}
			}
		}

		private void processVraag(VraagInstantieImpl vraagInstantie)
		{
			DefaultAntwoordKeuzeVraagDefinitieImpl<String> vraagDefinitie = (DefaultAntwoordKeuzeVraagDefinitieImpl<String>) vraagInstantie.getVraagDefinitie();
			String identifier = ((IdentifierElement) vraagDefinitie).getIdentifier();

			int vraagNummer = ++aantalVragenInDefinitie;

			boolean isVerplicht = vraagDefinitie.getVerplichting() != null;
			boolean isMeervoudig = vraagDefinitie.getRenderType().equals(AntwoordRenderType.CHECKBOX_HORIZONTAAL)
				|| vraagDefinitie.getRenderType().equals(AntwoordRenderType.CHECKBOX_VERTICAAL);
			boolean isZichtbaar = isZichtbaar(identifier);

			Set<Integer> gegevenAntwoorden = gegevenAntwoordenMap.get(vraagNummer);

			if (gegevenAntwoorden == null)
			{
				response.getFoutens().add(String.format("Vraag %d ontbreekt.", vraagNummer));
				return;
			}

			if (isZichtbaar)
			{
				if (isVerplicht && gegevenAntwoorden.size() == 0)
				{
					response.getFoutens().add(String.format("Vraag %d is verplicht.", vraagNummer));
				}
				if (!isMeervoudig && gegevenAntwoorden.size() > 1)
				{
					response.getFoutens().add(String.format("Op vraag %d is slechts één antwoord mogelijk.", vraagNummer));
				}
			}
			else
			{
				if (gegevenAntwoorden.size() > 0)
				{
					response.getFoutens().add(String.format("Vraag %d had niet moeten worden ingevuld.", vraagNummer));
				}
			}

			Set<String> teValiderenAntwoordenSet = new HashSet<>();
			if (!isMeervoudig)
			{
				if (!gegevenAntwoorden.isEmpty())
				{
					Integer antwoordNummer = gegevenAntwoorden.iterator().next();
					StringAntwoord stringAntwoord = new StringAntwoord();
					stringAntwoord.setVraagInstantie(vraagInstantie);
					String antwoordString = vraagDefinitie.getMogelijkeAntwoorden().get(antwoordNummer - 1).getAntwoordString();
					stringAntwoord.setValue(antwoordString);
					resultaatAntwoorden.add(stringAntwoord);
					teValiderenAntwoordenSet.add(antwoordString);
				}
			}
			else
			{
				if (!gegevenAntwoorden.isEmpty())
				{
					MeervoudigStringAntwoord meervoudigStringAntwoord = new MeervoudigStringAntwoord();
					meervoudigStringAntwoord.setVraagInstantie(vraagInstantie);
					for (Integer antwoordNummer : gegevenAntwoorden)
					{
						String antwoordString = vraagDefinitie.getMogelijkeAntwoorden().get(antwoordNummer - 1).getAntwoordString();
						meervoudigStringAntwoord.getValues().add(antwoordString);
						teValiderenAntwoordenSet.add(antwoordString);
					}
					resultaatAntwoorden.add(meervoudigStringAntwoord);
				}
			}
			teValiderenAntwoordenMap.put(identifier, teValiderenAntwoordenSet);
		}

		private boolean isZichtbaar(String targetIdentifier)
		{
			StringShowVraagActieInstantie actie = actieMap.get(targetIdentifier);
			if (actie != null)
			{
				String previousIdentifier = ((IdentifierElement) actie.getVraagInstantie().getVraagDefinitie()).getIdentifier();

				if (isZichtbaar(previousIdentifier))
				{
					Set<String> antwoorden = teValiderenAntwoordenMap.get(previousIdentifier);
					for (String antwoord : antwoorden)
					{
						if (actie.getActieResultaat(antwoord, null, null))
						{
							return true;
						}
					}
				}
				return false;
			}
			return true;
		}
	}
}
