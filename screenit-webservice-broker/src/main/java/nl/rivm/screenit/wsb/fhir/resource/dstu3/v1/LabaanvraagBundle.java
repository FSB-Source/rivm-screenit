package nl.rivm.screenit.wsb.fhir.resource.dstu3.v1;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import nl.rivm.screenit.dao.cervix.CervixHuisartsLocatieDao;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.util.CervixLocatieUtil;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixMonsterService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.hl7.fhir.dstu3.model.Bundle;
import org.hl7.fhir.dstu3.model.Coding;
import org.hl7.fhir.dstu3.model.Composition;
import org.hl7.fhir.dstu3.model.Identifier;
import org.hl7.fhir.dstu3.model.Narrative;
import org.hl7.fhir.dstu3.model.Organization;
import org.hl7.fhir.dstu3.model.Patient;
import org.hl7.fhir.dstu3.model.Practitioner;
import org.hl7.fhir.dstu3.model.Reference;
import org.hl7.fhir.dstu3.model.Resource;
import org.hl7.fhir.dstu3.model.ResourceType;
import org.hl7.fhir.utilities.xhtml.XhtmlNode;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Element;

import ca.uhn.fhir.model.api.annotation.ResourceDef;

@ResourceDef()
public class LabaanvraagBundle extends Bundle implements LabaanvraagResource
{
	private final String OBJ_ID = "D" + UUID.randomUUID().toString();

	private final ICurrentDateSupplier currentDateSupplier;

	private final CervixHuisartsLocatieDao locatieDao;

	private final CervixMonsterService monsterService;

	public LabaanvraagBundle()
	{
		this.currentDateSupplier = SpringBeanProvider.getInstance().getBean(ICurrentDateSupplier.class);
		this.locatieDao = SpringBeanProvider.getInstance().getBean(CervixHuisartsLocatieDao.class);
		this.monsterService = SpringBeanProvider.getInstance().getBean(CervixMonsterService.class);
	}

	public Date getScanDatum()
	{
		return getCurrentDate();
	}

	public String getObjid()
	{
		return OBJ_ID;
	}

	public BMHKLaboratorium getLaboratorium() throws NoSuchElementException
	{
		var uitstrijkje = getUitstrijkje();
		if (uitstrijkje != null && uitstrijkje.getLaboratorium() != null)
		{
			return uitstrijkje.getLaboratorium();
		}
		return getHuisartsLocatie()
			.getLocatieAdres()
			.getWoonplaats()
			.getGemeente()
			.getBmhkLaboratorium();
	}

	public CervixUitstrijkje getUitstrijkje()
	{
		if (getMonsterId() != null)
		{
			CervixUitstrijkje uitstrijkje = monsterService.getUitstrijkjeByClientBsnAndMonsterId(getClientBsn(), getMonsterId());
			if (uitstrijkje != null)
			{
				return uitstrijkje;
			}
		}
		return monsterService.getUitstrijkjeByClientBsnAndControleLetters(getClientBsn(), getControleLetters());
	}

	public CervixHuisartsLocatie getHuisartsLocatie() throws NoSuchElementException
	{
		final CervixHuisarts huisarts = getActieveHuisartsMetEenActieveLocatie();
		return huisarts
			.getHuisartsLocaties()
			.stream()
			.filter(locatie -> locatie.getStatus() == CervixLocatieStatus.ACTIEF && CervixLocatieUtil.isLocatieCompleet(locatie))
			.min(Comparator.comparing(CervixHuisartsLocatie::getId))
			.orElseThrow(() -> new NoSuchElementException(String.format("Voor huisarts met AGB code %s is geen actieve locatie gevonden.", huisarts.getAgbcode())));
	}

	private CervixHuisarts getActieveHuisartsMetEenActieveLocatie()
	{
		if (getIndividueleAgb() != null)
		{
			CervixHuisarts huisarts = getActieveHuisartsMetEenActieveLocatie(getIndividueleAgb());
			if (huisarts != null)
			{
				return huisarts;
			}
		}
		return getActieveHuisartsMetEenActieveLocatie(getPraktijkAgb());
	}

	private CervixHuisarts getActieveHuisartsMetEenActieveLocatie(String agbcode)
	{
		return locatieDao.getActieveHuisartsMetEenActieveLocatie(agbcode);
	}

	public CervixLabformulierStatus getStatus()
	{
		return CervixLabformulierStatus.GECONTROLEERD;
	}

	public Date getStatusDatum()
	{
		return getCurrentDate();
	}

	private Date getCurrentDate()
	{
		return currentDateSupplier.getDate();
	}

	@Override
	public String getClientBsn()
	{
		String reference = getCompositionStream()
			.map(composition -> composition.getSubject())
			.filter(Objects::nonNull)
			.map(subject -> subject.getReference())
			.findFirst().orElse(null);

		return getFromSystem(
			getResourceEntryComponentStream(reference, ResourceType.Patient)
				.map(entry -> ((Patient) entry.getResource()).getIdentifier()),
			CodeSystem.BSN);
	}

	@Override
	public String getPraktijkAgb()
	{
		String reference = getCompositionStream()
			.map(composition -> composition.getCustodian())
			.filter(Objects::nonNull)
			.map(custodian -> custodian.getReference())
			.findFirst().orElse(null);

		return getFromSystem(
			getResourceEntryComponentStream(reference, ResourceType.Organization)
				.map(entry -> ((Organization) entry.getResource()).getIdentifier()),
			CodeSystem.AGB);
	}

	@Override
	public String getIndividueleAgb()
	{
		List<Reference> authorReferences = getAuthorReferences();

		if (authorReferences.size() == 1)
		{
			return getFromSystem(
				getResourceEntryComponentStream(authorReferences.get(0).getReference(), ResourceType.Practitioner)
					.map(entry -> ((Practitioner) entry.getResource()).getIdentifier()),
				CodeSystem.AGB);
		}
		return null;
	}

	public List<Reference> getAuthorReferences()
	{
		return getCompositionStream()
			.map(composition -> composition.getAuthor())
			.filter(Objects::nonNull).findFirst().orElse(new ArrayList<>());
	}

	@Override
	public String getMonsterId()
	{
		return extractStringElementByCode(CodeSystem.MONSTER_ID);
	}

	@Override
	public String getControleLetters()
	{
		return extractStringElementByCode(CodeSystem.CONTROLELETTERS);
	}

	public String getDatumUitstrijkje()
	{
		return extractStringElementByCode(CodeSystem.DATUM_UITSTRIJKJE);
	}

	public List<String> getKlachten()
	{
		return extractListElementByCode(CodeSystem.KLACHTEN);
	}

	public String getKlachtenVrijeTekst()
	{
		return extractStringElementByCode(CodeSystem.KLACHTEN_VRIJE_TEKST);
	}

	public String getMenstruatie()
	{
		return extractStringElementByCode(CodeSystem.MENSTRUATIE);
	}

	public String getDatumLaatsteMenstruatie()
	{
		return extractStringElementByCode(CodeSystem.DATUM_LAATSTE_MENSTRUATIE);
	}

	public String getAnticonceptie()
	{
		return extractStringElementByCode(CodeSystem.ANTICONCEPTIE);
	}

	public List<String> getGebruikHormonen()
	{
		return extractListElementByCode(CodeSystem.GEBRUIK_HORMONEN);
	}

	public String getGebruikHormonenVrijeTekst()
	{
		return extractStringElementByCode(CodeSystem.GEBRUIK_HORMONEN_VRIJE_TEKST);
	}

	public String getAspectCervix()
	{
		return extractStringElementByCode(CodeSystem.ASPECT_CERVIX);
	}

	public String getAspectCervixVrijeTekst()
	{
		return extractStringElementByCode(CodeSystem.ASPECT_CERVIX_VRIJE_TEKST);
	}

	public String getOpmerkingenTekst()
	{
		return extractStringElementByCode(CodeSystem.OPMERKINGEN_TEKST);
	}

	public String getZorgDomeinID()
	{
		var identifierLijst = getCompositionStream().map(Composition::getIdentifier).collect(Collectors.toList());

		return identifierLijst.isEmpty() ? null : identifierLijst.get(0).getValue();
	}

	private Stream<Resource> getResourceStream()
	{
		return getEntry()
			.stream()
			.map(BundleEntryComponent::getResource)
			.filter(Objects::nonNull)
			.filter(resource -> Objects.nonNull(resource.getResourceType()));
	}

	private Stream<Composition> getCompositionStream()
	{
		return getResourceStream()
			.filter(resource -> resource.getResourceType().equals(ResourceType.Composition)).map(resource -> ((Composition) resource));
	}

	private Stream<Bundle.BundleEntryComponent> getEntryStreamWithResources()
	{
		return getEntry()
			.stream()
			.filter(entry -> Objects.nonNull(entry.getResource()) && Objects.nonNull(entry.getResource().getResourceType()));
	}

	private Stream<BundleEntryComponent> getResourceEntryComponentStream(String reference, ResourceType resourceType)
	{
		return getEntryStreamWithResources()
			.filter(entry -> entry.getResource().getResourceType().equals(resourceType) && entry.getFullUrl().equals(reference));
	}

	private String getFromSystem(Stream<List<Identifier>> identifiersStream, CodeSystem system)
	{
		return identifiersStream
			.map(identifiers -> getIdentifier(system, identifiers))
			.filter(Objects::nonNull)
			.map(Identifier::getValue)
			.findFirst()
			.orElse(null);
	}

	private Identifier getIdentifier(CodeSystem system, List<Identifier> identifiers)
	{
		return identifiers.stream()
			.filter(identifier -> Objects.nonNull(identifier.getSystem()))
			.filter(identifier -> identifier.getSystem().equalsIgnoreCase(system.getUrl()))
			.filter(identifier -> Objects.nonNull(identifier.getValue()))
			.filter(identifier -> !identifier.getValue().isEmpty())
			.findFirst()
			.orElse(null);
	}

	private String extractStringElementByCode(CodeSystem code)
	{
		return preProcessForElementExtraction(code)
			.map(XhtmlNode::getContent)
			.findFirst()
			.orElse(null);
	}

	public boolean correctlyMappedList(CodeSystem code)
	{
		return !(preProcessForElementExtraction(code).findAny().isPresent()
			&& extractListElementByCode(code).isEmpty());
	}

	private List<String> extractListElementByCode(CodeSystem code)
	{
		return preProcessForElementExtraction(code)
			.map(XhtmlNode::toString)
			.map(String::trim)
			.map(String::toLowerCase)
			.map(this::getUnorderedList)
			.findFirst()
			.orElse(Collections.emptyList());
	}

	private List<String> getUnorderedList(String content)
	{
		return Jsoup.parse(content)
			.getElementsByTag("li")
			.stream()
			.map(Element::text)
			.collect(Collectors.toList());
	}

	private Stream<XhtmlNode> preProcessForElementExtraction(CodeSystem code)
	{
		return getResourceStream()
			.filter(resource -> resource.getResourceType().equals(ResourceType.Composition))
			.map(resource -> ((Composition) resource).getSection())
			.filter(Objects::nonNull)
			.map(sectionComponents -> getSectionComponent(code, sectionComponents))
			.filter(Objects::nonNull)
			.map(Composition.SectionComponent::getText)
			.filter(Objects::nonNull)
			.map(Narrative::getDiv)
			.filter(Objects::nonNull)
			.map(XhtmlNode::getChildNodes)
			.filter(Objects::nonNull)
			.filter(childs -> !childs.isEmpty())
			.map(childs -> childs.get(0))
			.filter(Objects::nonNull);
	}

	private Composition.SectionComponent getSectionComponent(CodeSystem code,
		List<Composition.SectionComponent> sectionComponents)
	{
		List<Composition.SectionComponent> sectionComponentByCode = sectionComponents.stream()
			.filter(sectionComponent -> Objects.nonNull(sectionComponent.getCode()))
			.filter(sectionComponent -> Objects.nonNull(sectionComponent.getCode().getCoding()))
			.filter(sectionComponent -> matchesCodeSystem(code, sectionComponent)).collect(Collectors.toUnmodifiableList());

		if (sectionComponentByCode.size() > 1)
		{
			throw new IllegalArgumentException(String.format("vraag dubbel %s [%s]", code.name(), code.getCode()));
		}
		if (sectionComponentByCode.size() == 1)
		{
			return sectionComponentByCode.get(0);
		}

		return null;
	}

	private boolean matchesCodeSystem(CodeSystem code,
		Composition.SectionComponent sectionComponent)
	{
		return sectionComponent.getCode().getCoding().stream()
			.map(Coding::getCode)
			.filter(Objects::nonNull)
			.anyMatch(codingCode -> codingCode.equalsIgnoreCase(code.getCode()));
	}

}
