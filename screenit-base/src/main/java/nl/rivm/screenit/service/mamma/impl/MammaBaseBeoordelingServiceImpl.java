package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaArchitectuurverstoringLaesie;
import nl.rivm.screenit.model.mamma.MammaAsymmetrieLaesie;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaCalcificatiesLaesie;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaLaesie;
import nl.rivm.screenit.model.mamma.MammaLaesieIcoon;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaMassaLaesie;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeperktBeoordeelbaarReden;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaLezingType;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.repository.mamma.MammaBeoordelingRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseScreeningRondeService;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingReserveringService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseKwaliteitscontroleService;
import nl.rivm.screenit.service.mamma.MammaBaseLezingService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.service.mamma.MammaHuisartsBerichtService;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.functionalinterfaces.StringResolver;
import nl.rivm.screenit.util.mamma.MammaBeoordelingUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftDossier;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftLezing;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftUitslagStatus;

@Service
@Slf4j
public class MammaBaseBeoordelingServiceImpl implements MammaBaseBeoordelingService
{
	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private BerichtToBatchService hl7BerichtenToBatchService;

	@Autowired
	private MammaBaseBeoordelingReserveringService beoordelingReserveringService;

	@Autowired
	private MammaBaseLezingService lezingService;

	@Lazy
	@Autowired
	private MammaHuisartsBerichtService huisartsBerichtService;

	@Autowired
	private MammaBaseKansberekeningService kansberekeningService;

	@Autowired
	private MammaBaseFollowUpService followUpService;

	@Autowired
	private MammaBaseKwaliteitscontroleService baseKwaliteitscontroleService;

	@Autowired
	private MammaBaseOnderzoekService onderzoekService;

	@Autowired
	private BaseScreeningRondeService screeningrondeService;

	@Autowired
	private BaseScreeningRondeService baseScreeningRondeService;

	@Autowired
	private MammaVolgendeUitnodigingService volgendeUitnodigingService;

	@Autowired
	private MammaBeoordelingRepository beoordelingRepository;

	@Override
	public boolean isBiradsVerwijzen(MammaBIRADSWaarde biradsWaarde)
	{
		return MammaBIRADSWaarde.getVerwijzendBIRADSWaarden().contains(biradsWaarde);
	}

	@Override
	public boolean isBiradsNietVerwijzen(MammaBIRADSWaarde biradsWaarde)
	{
		return MammaBIRADSWaarde.getNietVerwijzendBIRADSWaarden().contains(biradsWaarde);
	}

	@Override
	public boolean isLezingVerwijzen(MammaLezing lezing)
	{
		return isBiradsVerwijzen(lezing.getBiradsLinks()) || isBiradsVerwijzen(lezing.getBiradsRechts());
	}

	@Override
	@Transactional
	public void bevestigLezing(MammaBeoordeling beoordeling)
	{
		bevestigLezing(beoordeling, true);
	}

	@Override
	@Transactional
	public void bevestigLezing(MammaBeoordeling beoordeling, boolean verstuurHl7Berichten)
	{
		if (MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus()))
		{
			setStatus(beoordeling, MammaBeoordelingStatus.TWEEDE_LEZING);
		}
		else if (MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus()))
		{
			if (verstuurHl7Berichten)
			{
				hl7BerichtenToBatchService.queueMammaHL7v24BerichtUitgaand(getClientVanBeoordeling(beoordeling), MammaHL7v24ORMBerichtStatus.REPORTED);
			}
			if (isBeoordelingDiscrepant(beoordeling))
			{
				setStatus(beoordeling, MammaBeoordelingStatus.DISCREPANTIE);
			}
			else
			{
				bepaalVervolgStapEnZetStatus(beoordeling, verstuurHl7Berichten);
			}
		}

		beoordelingReserveringService.geefBeoordelingVrij(beoordeling);
		hibernateService.saveOrUpdate(beoordeling);
	}

	private boolean isBeoordelingDiscrepant(MammaBeoordeling beoordeling)
	{
		var birads1 = getHoogsteBirads(beoordeling.getEersteLezing());
		var birads2 = getHoogsteBirads(beoordeling.getTweedeLezing());

		return isBiradsDiscrepant(birads1, birads2);
	}

	@Override
	public MammaBIRADSWaarde getHoogsteBirads(MammaLezing lezing)
	{
		if (lezing.getBiradsLinks() != null && lezing.getBiradsRechts() != null)
		{
			if (lezing.getBiradsLinks().getPrio() < lezing.getBiradsRechts().getPrio())
			{
				return lezing.getBiradsRechts();
			}
			else
			{
				return lezing.getBiradsLinks();
			}
		}
		else
		{
			throw new IllegalStateException("Lezing " + lezing + " is niet geldig.");
		}
	}

	@Override
	@Transactional
	public void opgeschortOnderzoekTerugNaarWerklijst(MammaBeoordeling beoordeling)
	{
		beoordeling.setOpschortGebruiker(null);
		beoordeling.setOpschortReden(MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN);
		beoordeling.setOpschortRedenTekst(null);
		setStatus(beoordeling, beoordeling.getEersteLezing() != null ? MammaBeoordelingStatus.TWEEDE_LEZING : MammaBeoordelingStatus.EERSTE_LEZING);
		hibernateService.saveOrUpdateAll(beoordeling);
	}

	private MammaBIRADSWaarde getBirads(MammaLezing lezing, MammaZijde zijde)
	{
		return MammaZijde.RECHTER_BORST.equals(zijde) ? lezing.getBiradsRechts() : lezing.getBiradsLinks();
	}

	@Override
	@Transactional
	public void discrepantieAfrondenEnNaarArbitrageZetten(MammaBeoordeling beoordeling, MammaLezing discrepantieLezing)
	{
		if (MammaBeoordelingStatus.DISCREPANTIE.equals(beoordeling.getStatus()))
		{
			discrepantieLezing.setBiradsRechts(MammaBIRADSWaarde.GEEN);
			discrepantieLezing.setBiradsLinks(MammaBIRADSWaarde.GEEN);
			koppelLezingAanBeoordeling(beoordeling, discrepantieLezing);
			setStatus(beoordeling, MammaBeoordelingStatus.ARBITRAGE);
			beoordelingReserveringService.geefBeoordelingVrij(beoordeling);
			hibernateService.saveOrUpdate(beoordeling);
		}
	}

	@Override
	@Transactional
	public void setStatusNaarVerslagGereed(MammaBeoordeling beoordeling)
	{
		if (MammaBeoordelingStatus.VERSLAG_MAKEN.equals(beoordeling.getStatus()) || MammaBeoordelingStatus.VERSLAG_AFGEKEURD.equals(beoordeling.getStatus()))
		{
			setStatus(beoordeling, MammaBeoordelingStatus.VERSLAG_GEREED);
			beoordelingReserveringService.geefBeoordelingVrij(beoordeling);
			beoordeling.setAfkeurreden(null);
			beoordeling.setToegewezenOp(null);
			beoordeling.setToegewezenGebruiker(null);
		}
		hibernateService.saveOrUpdate(beoordeling);
	}

	@Override
	public boolean isDiscrepantieVerwijzingVerplicht(MammaBeoordeling beoordeling, MammaZijde zijde)
	{
		return MammaBeoordelingStatus.DISCREPANTIE.equals(beoordeling.getStatus()) && isBiradsDiscrepantOpZijde(beoordeling, zijde);
	}

	private boolean isBiradsDiscrepantOpZijde(MammaBeoordeling beoordeling, MammaZijde zijde)
	{
		var birads1 = getBirads(beoordeling.getEersteLezing(), zijde);
		var birads2 = getBirads(beoordeling.getTweedeLezing(), zijde);

		return isBiradsDiscrepant(birads1, birads2);
	}

	public boolean isBiradsDiscrepant(MammaBIRADSWaarde birads1, MammaBIRADSWaarde birads2)
	{
		return isBiradsVerwijzen(birads1) && isBiradsNietVerwijzen(birads2) || isBiradsVerwijzen(birads2) && isBiradsNietVerwijzen(birads1);
	}

	@Override
	public MammaBIRADSWaarde defaultBiradsWaarde(MammaBeoordeling beoordeling, MammaZijde zijde)
	{
		if (MammaBeoordelingStatus.EERSTE_LEZING.equals(beoordeling.getStatus()) || MammaBeoordelingStatus.TWEEDE_LEZING.equals(beoordeling.getStatus()))
		{
			return lezingService.isZijdeGeamputeerd(zijde, beoordeling.getOnderzoek().getAmputatie()) ? MammaBIRADSWaarde.GEEN : MammaBIRADSWaarde.EEN;
		}
		else if (MammaBeoordelingStatus.DISCREPANTIE.equals(beoordeling.getStatus()))
		{
			return defaultDiscrepantieBiradsWaarde(beoordeling, zijde);
		}
		else if (MammaBeoordelingStatus.ARBITRAGE.equals(beoordeling.getStatus()))
		{
			return lezingService.isZijdeGeamputeerd(zijde, beoordeling.getOnderzoek().getAmputatie()) ? MammaBIRADSWaarde.GEEN : null;
		}
		return MammaBIRADSWaarde.EEN;
	}

	@Override
	@Transactional
	public void slaLezingOpEnVerwerkStatus(MammaBeoordeling beoordeling, MammaLezing lezing, InstellingGebruiker gebruiker, StringResolver stringResolverMethod)
	{
		wisBiradsBijOnbeoordeelbaar(lezing);

		if (MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN.equals(beoordeling.getOpschortReden()))
		{
			slaLezingOp(beoordeling, lezing);
			verwerkStatusNaOpslaanLezing(beoordeling);
		}
		else
		{
			if (lezing.getLezingType() == MammaLezingType.EERSTE_LEZING && beoordeling.getEersteLezing() != null)
			{
				hibernateService.delete(beoordeling.getEersteLezing());
				beoordeling.setEersteLezing(null);
			}
			if (lezing.getLezingType() == MammaLezingType.TWEEDE_LEZING && beoordeling.getTweedeLezing() != null)
			{
				hibernateService.delete(beoordeling.getTweedeLezing());
				beoordeling.setTweedeLezing(null);
			}
			hibernateService.saveOrUpdate(beoordeling);
			verwerkOpschortingBeoordeling(beoordeling, gebruiker, stringResolverMethod);
		}
	}

	private void wisBiradsBijOnbeoordeelbaar(MammaLezing lezing)
	{
		if (lezing.getBeperktBeoordeelbaarReden() == MammaBeperktBeoordeelbaarReden.GEEN_BEOORDELING_MOGELIJK)
		{
			lezing.setBiradsLinks(MammaBIRADSWaarde.GEEN);
			lezing.setBiradsRechts(MammaBIRADSWaarde.GEEN);
		}
	}

	@Override
	@Transactional
	public void verwerkBeoordelingStatusGunstigMetNevenbevindingen(MammaBeoordeling beoordeling)
	{
		bepaalVervolgStapEnZetStatus(beoordeling, true);
	}

	private void verwerkOpschortingBeoordeling(MammaBeoordeling beoordeling, InstellingGebruiker gebruiker, StringResolver stringResolverMethod)
	{
		setStatus(beoordeling, MammaBeoordelingStatus.OPGESCHORT);
		beoordeling.setOpschortGebruiker(gebruiker);

		logService.logGebeurtenis(LogGebeurtenis.MAMMA_BEOORDELING_OPGESCHORT, gebruiker,
			getClientVanBeoordeling(beoordeling), "Beoordeling opgeschort met reden: " + stringResolverMethod.resolveString(beoordeling), Bevolkingsonderzoek.MAMMA);
		hibernateService.saveOrUpdate(beoordeling);
	}

	@Override
	@Transactional
	public void slaLezingOp(MammaBeoordeling beoordeling, MammaLezing lezing)
	{
		koppelLezingAanBeoordeling(beoordeling, lezing);

		hibernateService.saveOrUpdate(lezing);
		hibernateService.saveOrUpdate(beoordeling);

		logService.logGebeurtenis(LogGebeurtenis.MAMMA_BEOORDELING_AFGEROND, lezing.getBeoordelaar(),
			getClientVanBeoordeling(beoordeling),
			beoordeling.getStatus().getNaam() + MammaScreeningRondeUtil.bepaalNaamBiradsWaarde(MammaZijde.RECHTER_BORST, lezing.getBiradsRechts())
				+ MammaScreeningRondeUtil.bepaalNaamBiradsWaarde(MammaZijde.LINKER_BORST, lezing.getBiradsLinks()) + getLogMeldingOnderzoekTypeTekst(
				beoordeling),
			Bevolkingsonderzoek.MAMMA);
	}

	private String getLogMeldingOnderzoekTypeTekst(MammaBeoordeling beoordeling)
	{
		return beoordeling.getOnderzoek().getOnderzoekType() == MammaOnderzoekType.MAMMOGRAFIE ? "" : " (" + beoordeling.getOnderzoek().getOnderzoekType().getNaam() + ")";
	}

	private void koppelLezingAanBeoordeling(MammaBeoordeling beoordeling, MammaLezing lezing)
	{
		if (MammaLezingType.DISCREPANTIE_LEZING.equals(lezing.getLezingType()) || MammaLezingType.ARBITRAGE_LEZING.equals(lezing.getLezingType()))
		{
			valideerGeldigeBeoordelaarDiscrepantieOfArbitrage(beoordeling, lezing);
		}

		lezing.setBeoordelingDatum(currentDateSupplier.getDate());

		if (MammaLezingType.EERSTE_LEZING.equals(lezing.getLezingType()))
		{
			beoordeling.setEersteLezing(lezing);
		}
		else if (MammaLezingType.TWEEDE_LEZING.equals(lezing.getLezingType()))
		{
			beoordeling.setTweedeLezing(lezing);
		}
		else if (MammaLezingType.VERSLAG_LEZING.equals(lezing.getLezingType()))
		{
			beoordeling.setVerslagLezing(lezing);
		}
		else if (MammaLezingType.DISCREPANTIE_LEZING.equals(lezing.getLezingType()))
		{
			beoordeling.setDiscrepantieLezing(lezing);
		}
		else if (MammaLezingType.ARBITRAGE_LEZING.equals(lezing.getLezingType()))
		{
			beoordeling.setArbitrageLezing(lezing);
		}
		else
		{
			throw new IllegalArgumentException(
				"Beoordeling met het lezingtype " + lezing.getLezingType() + " is niet gekoppeld aan een verslag en is daarom niet opgeslagen.");
		}
	}

	private void valideerGeldigeBeoordelaarDiscrepantieOfArbitrage(MammaBeoordeling beoordeling, MammaLezing lezing)
	{
		var huidigeBeoordelaar = lezing.getBeoordelaar();
		var isBeoordeeldDoorEersteOfTweedeBeoordelaar = beoordeling.getEersteLezing().getBeoordelaar().equals(huidigeBeoordelaar) ||
			beoordeling.getTweedeLezing().getBeoordelaar().equals(huidigeBeoordelaar);

		var discrepantieSituatie = MammaLezingType.DISCREPANTIE_LEZING.equals(lezing.getLezingType())
			&& (isBeoordeeldDoorEersteOfTweedeBeoordelaar || huidigeBeoordelaar == null);
		var arbitrageSituatie = MammaLezingType.ARBITRAGE_LEZING.equals(lezing.getLezingType()) && !isBeoordeeldDoorEersteOfTweedeBeoordelaar;

		if (!discrepantieSituatie && !arbitrageSituatie)
		{
			throw new IllegalStateException("De beoordelaar van lezing " + lezing.getId() + " is ongeldig");
		}
	}

	@Override
	public MammaScreeningRonde getScreeningRonde(MammaBeoordeling beoordeling)
	{
		return beoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde();
	}

	@Override
	public Optional<MammaBeoordeling> getBeoordelingVanLezing(MammaLezing lezing)
	{
		return beoordelingRepository.findOne(heeftLezing(lezing));
	}

	@Override
	public Client getClientVanBeoordeling(MammaBeoordeling beoordeling)
	{
		return getScreeningRonde(beoordeling).getDossier().getClient();
	}

	@Override
	@Transactional
	public void setStatus(MammaBeoordeling beoordeling, MammaBeoordelingStatus status)
	{
		beoordeling.setStatus(status);
		beoordeling.setStatusDatum(currentDateSupplier.getDate());
		if (MammaBeoordelingStatus.isUitslagStatus(status))
		{
			var dossier = beoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier();
			dossier.setLaatsteBeoordelingMetUitslag(beoordeling);
			hibernateService.saveOrUpdate(dossier);
			followUpService.refreshUpdateFollowUpConclusie(dossier);
			if (status == MammaBeoordelingStatus.UITSLAG_ONGUNSTIG)
			{
				volgendeUitnodigingService.updateVolgendeUitnodigingNaVerwijziging(dossier);
			}
		}
	}

	@Override
	public void wijsBeoordelingAanRadioloogToe(MammaBeoordeling beoordeling, InstellingGebruiker gebruiker)
	{
		beoordeling.setToegewezenGebruiker(gebruiker);
		beoordeling.setToegewezenOp(currentDateSupplier.getDate());
		String melding;
		var vorigeBeoordeling = EntityAuditUtil.getLastVersionOfEntity(beoordeling, hibernateService.getHibernateSession());
		if (beoordeling.getVerslagLezing() != null && vorigeBeoordeling != null)
		{
			if (vorigeBeoordeling.getToegewezenGebruiker() != null && vorigeBeoordeling.getToegewezenOp() != null)
			{
				melding = String.format("Verslaglezing van radioloog %s, toegewezen aan radioloog: %s. Op %s",
					NaamUtil.getNaamGebruiker(vorigeBeoordeling.getToegewezenGebruiker().getMedewerker()),
					NaamUtil.getNaamGebruiker(gebruiker.getMedewerker()),
					Constants.getDateTimeFormat().format(beoordeling.getToegewezenOp()));
			}
			else
			{
				melding = String.format("Verslaglezing van originele radioloog %s, toegewezen aan radioloog: %s. Op %s",
					NaamUtil.getNaamGebruiker(vorigeBeoordeling.getVerslagLezing().getBeoordelaar().getMedewerker()),
					NaamUtil.getNaamGebruiker(gebruiker.getMedewerker()),
					Constants.getDateTimeFormat().format(beoordeling.getToegewezenOp()));
			}
		}
		else
		{
			melding = String.format("Verslaglezing van beoordeling toegewezen aan radioloog %s, op %s", NaamUtil.getNaamGebruiker(gebruiker.getMedewerker()),
				Constants.getDateTimeFormat().format(beoordeling.getToegewezenOp()));
		}
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_CE_RADIOLOOG_TOEGEWEZEN_AAN_VERSLAGLEZING, new LogEvent(melding),
			beoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient(), Bevolkingsonderzoek.MAMMA);
	}

	private MammaBIRADSWaarde defaultDiscrepantieBiradsWaarde(MammaBeoordeling beoordeling, MammaZijde zijde)
	{
		if (isBiradsDiscrepantOpZijde(beoordeling, zijde))
		{
			return null;
		}
		else
		{
			var biradsEen = getBirads(beoordeling.getEersteLezing(), zijde);
			var biradsTwee = getBirads(beoordeling.getTweedeLezing(), zijde);
			return biradsEen.equals(biradsTwee) ? biradsEen : null;
		}
	}

	private void verwerkStatusNaOpslaanLezing(MammaBeoordeling beoordeling)
	{
		if (MammaBeoordelingStatus.EERSTE_LEZING.equals(beoordeling.getStatus()))
		{
			setStatus(beoordeling, MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN);
		}
		else if (MammaBeoordelingStatus.TWEEDE_LEZING.equals(beoordeling.getStatus()))
		{
			setStatus(beoordeling, MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN);
		}
		else if (MammaBeoordelingStatus.DISCREPANTIE.equals(beoordeling.getStatus()) || MammaBeoordelingStatus.ARBITRAGE.equals(beoordeling.getStatus()))
		{
			bepaalVervolgStapEnZetStatus(beoordeling, true);
			beoordelingReserveringService.geefBeoordelingVrij(beoordeling);
		}
		else if (MammaBeoordelingStatus.VERSLAG_MAKEN == beoordeling.getStatus())
		{
			var errorString = String.format("MammaBeoordeling met status %s en id '%d' zou hier niet moeten komen.", beoordeling.getStatus(), beoordeling.getId());
			throw new IllegalStateException(errorString);
		}

		hibernateService.saveOrUpdate(beoordeling);
	}

	private void bepaalVervolgStapEnZetStatus(MammaBeoordeling beoordeling, boolean verstuurHl7Berichten)
	{
		if (isConclusieVerwijzen(beoordeling))
		{
			setStatus(beoordeling, MammaBeoordelingStatus.VERSLAG_MAKEN);
		}
		else
		{
			var heeftOnderzoekNevenbevindingen = heeftOnderzoekNevenbevindingen(beoordeling);
			if (heeftOnderzoekNevenbevindingen && (MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus()) ||
				MammaBeoordelingStatus.ARBITRAGE.equals(beoordeling.getStatus())))
			{
				setStatus(beoordeling, MammaBeoordelingStatus.GUNSTIG_MET_NEVENBEVINDING);
			}
			else
			{
				setStatus(beoordeling, MammaBeoordelingStatus.UITSLAG_GUNSTIG);
				if (verstuurHl7Berichten)
				{
					hl7BerichtenToBatchService.queueMammaHL7v24BerichtUitgaand(getClientVanBeoordeling(beoordeling), MammaHL7v24ORMBerichtStatus.AUTHORISED);
				}

				var ronde = getScreeningRonde(beoordeling);
				final var beperktBeoordeelbaarReden = MammaBeoordelingUtil.beperktBeoordeelbaarReden(beoordeling);
				if (beperktBeoordeelbaarReden == null)
				{
					maakGunstigeUitlagBrief(ronde, BriefType.MAMMA_GUNSTIGE_UITSLAG, false);
				}
				else
				{
					switch (beperktBeoordeelbaarReden)
					{
					case FOTOS_MAAR_IN_1_RICHTING_GEMAAKT:
					case MAMMA_NIET_VOLLEDIG_AFGEBEELD:
						maakGunstigeUitlagBrief(ronde, BriefType.MAMMA_BEPERKT_BEOORDEELBAAR, false);
						break;
					case PROTHESE_MEER_DAN_0_PUNT_8:
						var gunstigeUitslagBriefIsGemaakt = maakGunstigeUitlagBrief(ronde, BriefType.MAMMA_BEPERKT_BEOORDEELBAAR_PROTHESE, false);
						final var huisarts = beoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getHuisarts();
						if (huisarts != null && gunstigeUitslagBriefIsGemaakt)
						{
							huisartsBerichtService.verstuurHuisartsBericht(beoordeling, huisarts, HuisartsBerichtType.MAMMA_PROTHESE_MEER_DAN_80_PROCENT, false);
						}
						break;
					case GEEN_BEOORDELING_MOGELIJK:
						setStatus(beoordeling, MammaBeoordelingStatus.ONBEOORDEELBAAR_TE_VERSTUREN);
						maakGunstigeUitlagBrief(ronde, BriefType.MAMMA_GEEN_BEOORDELING_MOGELIJK, true);
						break;
					case FOTOS_VAN_EEN_OF_BEIDE_ZIJDEN_BEWOGEN:
					case KWALITEIT_KOMT_NIET_OVEREEN_MET_STANDAARD:
						maakGunstigeUitlagBrief(ronde, BriefType.MAMMA_GUNSTIGE_UITSLAG, false);
						break;
					default:
						throw new IllegalStateException("Beperkt beoordeelbaar reden: " + beperktBeoordeelbaarReden + " bestaat niet.");
					}
				}
				if (beoordeling.getStatus() != MammaBeoordelingStatus.ONBEOORDEELBAAR_TE_VERSTUREN)
				{
					baseScreeningRondeService.screeningRondeAfronden(ronde);
				}
				if (beoordeling.getStatus() == MammaBeoordelingStatus.UITSLAG_GUNSTIG)
				{
					kansberekeningService.dossierEventHerzien(ronde.getDossier());
				}
			}
		}
	}

	private boolean maakGunstigeUitlagBrief(MammaScreeningRonde ronde, BriefType briefType, boolean briefGegenereerd)
	{
		if (!briefService.briefTypeAlVerstuurdInDezeRonde(ronde, Collections.singletonList(briefType)))
		{
			briefService.maakBvoBrief(ronde, briefType, briefGegenereerd);
			return true;
		}
		return false;
	}

	private boolean heeftOnderzoekNevenbevindingen(MammaBeoordeling beoordeling)
	{
		return beoordeling.getEersteLezing() != null && beoordeling.getTweedeLezing() != null
			&& (!beoordeling.getEersteLezing().getNevenbevindingen().isEmpty() || !beoordeling.getTweedeLezing().getNevenbevindingen().isEmpty());
	}

	boolean isConclusieVerwijzen(MammaBeoordeling beoordeling)
	{
		if (beoordeling.getArbitrageLezing() != null)
		{
			return isBiradsVerwijzen(getHoogsteBirads(beoordeling.getArbitrageLezing()));
		}
		else if (beoordeling.getDiscrepantieLezing() != null)
		{
			return isBiradsVerwijzen(getHoogsteBirads(beoordeling.getDiscrepantieLezing()));
		}
		else
		{
			if (isBiradsVerwijzen(getHoogsteBirads(beoordeling.getEersteLezing())) != isBiradsVerwijzen(getHoogsteBirads(beoordeling.getTweedeLezing())))
			{
				throw new IllegalStateException("Beoordeling " + beoordeling + " is ongeldig.");
			}
			return isBiradsVerwijzen(getHoogsteBirads(beoordeling.getEersteLezing())) && isBiradsVerwijzen(getHoogsteBirads(beoordeling.getTweedeLezing()));
		}
	}

	@Override
	public MammaLezing maakVerslagLezing(MammaBeoordeling beoordeling, MammaLezing uitgangsituatieLezing, InstellingGebruiker beoordelaar, boolean onervarenRadioloog)
	{
		var verslagLezing = new MammaLezing();
		verslagLezing.setLezingType(MammaLezingType.VERSLAG_LEZING);
		var mammaLaesies = createMammaLaesies(uitgangsituatieLezing, verslagLezing);
		verslagLezing.setLaesies(mammaLaesies);
		verslagLezing.setBeoordelaar(beoordelaar);
		verslagLezing.setOnervarenRadioloog(onervarenRadioloog);
		verslagLezing.setBiradsRechts(uitgangsituatieLezing.getBiradsRechts());
		verslagLezing.setBiradsLinks(uitgangsituatieLezing.getBiradsLinks());
		return verslagLezing;
	}

	private List<MammaLaesie> createMammaLaesies(MammaLezing uitgangsituatieLezing, MammaLezing verslagLezing)
	{
		List<MammaLaesie> mammaLaesies = new ArrayList<>();
		for (var mammaLaesie : uitgangsituatieLezing.getLaesies())
		{
			var kopieMammaLaesie = cloneLaesie(mammaLaesie);
			kopieMammaLaesie.setLezing(verslagLezing);
			mammaLaesies.add(kopieMammaLaesie);
		}
		return mammaLaesies;
	}

	private MammaLaesie cloneLaesie(MammaLaesie mammaLaesie)
	{
		var kopieMammaLaesie = createLaesie(mammaLaesie.getMammaLaesieType());
		kopieMammaLaesie.setMammaZijde(mammaLaesie.getMammaZijde());
		kopieMammaLaesie.setNummer(mammaLaesie.getNummer());
		if (mammaLaesie.getHorizontaleDoorsnedeIcoon() != null)
		{
			var mammaLaesieIcoonHorizontaal = cloneMammaLaesieIcoon(mammaLaesie.getHorizontaleDoorsnedeIcoon());
			kopieMammaLaesie.setHorizontaleDoorsnedeIcoon(mammaLaesieIcoonHorizontaal);
		}
		if (mammaLaesie.getVerticaleDoorsnedeIcoon() != null)
		{
			var mammaLaesieIcoonVerticaal = cloneMammaLaesieIcoon(mammaLaesie.getVerticaleDoorsnedeIcoon());
			kopieMammaLaesie.setVerticaleDoorsnedeIcoon(mammaLaesieIcoonVerticaal);
		}
		return kopieMammaLaesie;
	}

	private MammaLaesie createLaesie(MammaLaesieType mammaLaesieType)
	{
		switch (mammaLaesieType)
		{
		case ARCHITECTUURVERSTORING:
			return new MammaArchitectuurverstoringLaesie();
		case ASYMMETRIE:
			return new MammaAsymmetrieLaesie();
		case CALCIFICATIES:
			return new MammaCalcificatiesLaesie();
		case MASSA:
			return new MammaMassaLaesie();
		default:
			throw new IllegalArgumentException("Onbekend laesieType: " + mammaLaesieType);
		}
	}

	private MammaLaesieIcoon cloneMammaLaesieIcoon(MammaLaesieIcoon mammaLaesieIcoon)
	{
		var kopieMammaLaesieIcoon = new MammaLaesieIcoon();
		kopieMammaLaesieIcoon.setPositieX(mammaLaesieIcoon.getPositieX());
		kopieMammaLaesieIcoon.setPositieY(mammaLaesieIcoon.getPositieY());
		return kopieMammaLaesieIcoon;
	}

	@Override
	public Boolean isUitslagGunstig(MammaBeoordeling beoordeling)
	{
		if (beoordeling != null
			&& (MammaBeoordelingStatus.UITSLAG_GUNSTIG.equals(beoordeling.getStatus()) || MammaBeoordelingStatus.UITSLAG_ONGUNSTIG.equals(beoordeling.getStatus())))
		{
			return MammaBeoordelingStatus.UITSLAG_GUNSTIG.equals(beoordeling.getStatus());
		}

		return null;
	}

	@Override
	public MammaBIRADSWaarde getResultaatVoorZijde(MammaBeoordeling beoordeling, MammaZijde zijde)
	{
		var uitslagGunstig = isUitslagGunstig(beoordeling);
		if (uitslagGunstig == null)
		{
			return null;
		}
		else if (!uitslagGunstig)
		{
			return getBirads(beoordeling.getVerslagLezing(), zijde);
		}
		else if (beoordeling.getArbitrageLezing() != null)
		{
			return getBirads(beoordeling.getArbitrageLezing(), zijde);
		}
		else if (beoordeling.getDiscrepantieLezing() != null) 
		{
			return getBirads(beoordeling.getDiscrepantieLezing(), zijde);
		}
		else
		{
			return getHoogsteBiradsVoorZijde(beoordeling, zijde);
		}
	}

	@Override
	public boolean iBiradsWaardeGeen(MammaLezing verslagLezing, MammaZijde zijde)
	{
		var biradsWaarde = getBirads(verslagLezing, zijde);
		return MammaBIRADSWaarde.GEEN.equals(biradsWaarde);
	}

	private MammaBIRADSWaarde getHoogsteBiradsVoorZijde(MammaBeoordeling beoordeling, MammaZijde zijde)
	{
		try
		{

			if (MammaZijde.RECHTER_BORST.equals(zijde))
			{
				return MammaBIRADSWaarde.getHoogsteBiradsUitTweeWaarden(beoordeling.getEersteLezing().getBiradsRechts(), beoordeling.getTweedeLezing().getBiradsRechts());
			}
			else
			{
				return MammaBIRADSWaarde.getHoogsteBiradsUitTweeWaarden(beoordeling.getEersteLezing().getBiradsLinks(), beoordeling.getTweedeLezing().getBiradsLinks());
			}
		}
		catch (NullPointerException ex)
		{
			LOG.error("Null Lezing voor beoordeling id: {} ", beoordeling.getId());
			return null;
		}
	}

	@Override
	public MammaLezing getOrineleVerslagLezing(MammaBeoordeling beoordeling)
	{
		var entityHistory = EntityAuditUtil.getEntityHistory(beoordeling, hibernateService.getHibernateSession(), true);
		for (var i = entityHistory.size(); i > 0; i--)
		{
			MammaBeoordeling beoordelingRev = EntityAuditUtil.getRevisionEntity(entityHistory.get(i - 1));
			if (beoordelingRev.getToegewezenGebruiker() != null && beoordelingRev.getToegewezenOp() != null)
			{
				return null;
			}
			if (beoordelingRev.getVerslagLezing() != null && beoordelingRev.getVerslagLezing().getBeoordelaar() != null)
			{
				return beoordelingRev.getVerslagLezing();
			}
		}
		return null;
	}

	@Override
	public String getMammaLezingEnumsTekst(Function<MammaLezing, List<? extends INaam>> getEnumListFromLezing, MammaLezing... lezingen)
	{
		if (Arrays.stream(lezingen).noneMatch(Objects::isNull))
		{
			return StringUtils.capitalize(
				Arrays.stream(lezingen)
					.flatMap(lezing -> getEnumListFromLezing.apply(lezing).stream())
					.distinct()
					.map(INaam::getNaam)
					.map(StringUtils::lowerCase)
					.collect(Collectors.joining(", ")));
		}
		else
		{
			return "";
		}
	}

	@Override
	public List<String> getNevenBevindingenOpmerkingenAsList(MammaBeoordeling beoordeling)
	{
		var opmerkingen = new ArrayList<String>();
		if (beoordeling != null && beoordeling.getEersteLezing() != null && beoordeling.getTweedeLezing() != null)
		{
			if (beoordeling.getEersteLezing().getNevenbevindingOpmerking() != null)
			{
				opmerkingen.add(beoordeling.getEersteLezing().getNevenbevindingOpmerking());
			}
			if (beoordeling.getTweedeLezing().getNevenbevindingOpmerking() != null)
			{
				opmerkingen.add(beoordeling.getTweedeLezing().getNevenbevindingOpmerking());
			}
		}
		return opmerkingen;
	}

	@Override
	public boolean heeftBeoordelingNevenbevindingen(MammaBeoordeling beoordeling)
	{
		return beoordeling.getEersteLezing() != null && beoordeling.getTweedeLezing() != null
			&& (!beoordeling.getEersteLezing().getNevenbevindingen().isEmpty() || !beoordeling.getTweedeLezing().getNevenbevindingen().isEmpty());
	}

	@Override
	public String getNevenbevindingOpmerkingTekst(String lineBreak, MammaLezing... lezingen)
	{
		if (lezingen != null && lezingen.length > 0)
		{
			var opmerkingen = Arrays.stream(lezingen)
				.filter(Objects::nonNull)
				.map(MammaLezing::getNevenbevindingOpmerking)
				.filter(StringUtils::isNotBlank)
				.collect(Collectors.joining(lineBreak));
			return StringUtils.isBlank(opmerkingen) ? null : opmerkingen;
		}
		return null;
	}

	@Override
	public MammaBeoordeling getBeoordelingMetVerslagLezing(MammaAfspraak afspraak)
	{
		if (afspraak != null && afspraak.getOnderzoek() != null)
		{
			return afspraak.getOnderzoek().getBeoordelingen().stream()
				.filter(b -> b.getVerslagLezing() != null)
				.map(b ->
				{
					b.getVerslagLezing().setBeoordeling(b);
					return b.getVerslagLezing();
				})
				.max(Comparator.comparing(MammaLezing::getBeoordelingDatum))
				.orElse(new MammaLezing()).getBeoordeling();
		}
		return null;
	}

	@Override
	public MammaBeoordeling getBeoordelingMetEersteEnOfTweedeLezing(MammaAfspraak afspraak)
	{
		if (afspraak != null && afspraak.getOnderzoek() != null)
		{
			return afspraak.getOnderzoek().getBeoordelingen().stream()
				.filter(b -> b.getEersteLezing() != null || b.getTweedeLezing() != null)
				.map(b ->
				{
					if (b.getTweedeLezing() != null)
					{
						b.getTweedeLezing().setBeoordeling(b);
						return b.getTweedeLezing();
					}
					else
					{
						b.getEersteLezing().setBeoordeling(b);
						return b.getEersteLezing();
					}
				})
				.max(Comparator.comparing(MammaLezing::getBeoordelingDatum))
				.map(MammaLezing::getBeoordeling).get();
		}
		return null;
	}

	@Override
	public Optional<MammaBeoordeling> zoekOpgeschorteBeoordelingInRonde(MammaScreeningRonde ronde, MammaBeoordelingOpschortenReden... opschortenRedenen)
	{
		return ronde.getUitnodigingen().stream()
			.flatMap(uitnoding -> uitnoding.getAfspraken().stream())
			.filter(afspraak -> afspraak.getOnderzoek() != null)
			.flatMap(afspraak -> afspraak.getOnderzoek().getBeoordelingen().stream())
			.filter(beoordeling -> Stream.of(opschortenRedenen).anyMatch(reden -> reden.equals(beoordeling.getOpschortReden())))
			.min(Comparator.comparing(MammaBeoordeling::getStatusDatum));
	}

	@Override
	@Transactional
	public void annuleerBeoordeling(MammaBeoordeling beoordeling)
	{
		setStatus(beoordeling, MammaBeoordelingStatus.GEANNULEERD);
		hibernateService.saveOrUpdate(beoordeling);

		var dossier = beoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier();
		var laatsteBeoordelingMetUitslag = getLaatsteBeoordelingMetUitslag(dossier);
		dossier.setLaatsteBeoordelingMetUitslag(laatsteBeoordelingMetUitslag);
		hibernateService.saveOrUpdate(dossier);
		followUpService.refreshUpdateFollowUpConclusie(dossier);
	}

	@Override
	@Transactional
	public void valideerEnHerbeoordeelBeoordeling(MammaBeoordeling laatsteBeoordeling, InstellingGebruiker ingelogdeGebruiker)
	{
		if (beoordelingReserveringService.gereserveerdDoorIemandAnders(ingelogdeGebruiker, laatsteBeoordeling))
		{
			throw new IllegalStateException("Een radioloog is bezig met een lezing voor deze beoordeling, wacht met annuleren tot de lezing is opgeslagen.");
		}
		if (beoordelingZitInActieveFotobespreking(laatsteBeoordeling))
		{
			throw new IllegalStateException("De beoordeling is meegenomen in een fotobespreking");
		}
		else
		{
			herbeoordeelBeoordeling(laatsteBeoordeling);
		}
	}

	@Override
	public boolean beoordelingZitInActieveFotobespreking(MammaBeoordeling beoordeling)
	{
		var ronde = getScreeningRonde(beoordeling);
		var onderzoeken = baseKwaliteitscontroleService.getFotobesprekingOnderzoeken(ronde);
		return onderzoeken.stream().anyMatch(onderzoek -> onderzoek.getFotobespreking().getAfgerondOp() == null && onderzoek.getBeoordeling().equals(beoordeling));
	}

	private void herbeoordeelBeoordeling(MammaBeoordeling laatsteBeoordeling)
	{
		annuleerBeoordeling(laatsteBeoordeling);
		var ronde = getScreeningRonde(laatsteBeoordeling);
		screeningrondeService.heropenScreeningRonde(ronde);
		onderzoekService.voegNieuweBeoordelingToe(laatsteBeoordeling.getOnderzoek());

		briefService.setNietGegenereerdeBrievenOpTegenhouden(ronde, BriefType.getMammaGunstigeUitslagBriefTypen());
	}

	@Override
	public MammaBeoordeling getLaatsteBeoordelingMetUitslag(MammaDossier dossier)
	{
		return beoordelingRepository.findFirst(heeftDossier(dossier).and(heeftUitslagStatus()),
				Sort.by(Sort.Direction.DESC, MammaBeoordeling_.STATUS_DATUM))
			.orElse(null);
	}

}
