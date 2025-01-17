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

import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaAfmeldService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.model.AfmeldingType.DEFINITIEF;

@Component
@Transactional(propagation = Propagation.REQUIRED)
@Slf4j
public class MammaAfmeldServiceImpl implements MammaAfmeldService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private MammaBaseAfspraakService baseAfspraakService;

	@Autowired
	private MammaBaseUitstelService baseUitstelService;

	@Autowired
	private MammaBaseScreeningrondeService screeningRondeService;

	@Autowired
	private MammaBaseDossierService dossierService;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public MammaAfmelding maakAfmelding()
	{
		return new MammaAfmelding();
	}

	@Override
	public void definitieveAfmeldingAanvragen(MammaAfmelding afmelding, boolean rappelBrief)
	{
		var briefType = BriefType.MAMMA_AFMELDING_AANVRAAG;
		if (rappelBrief)
		{
			briefType = BriefType.MAMMA_AFMELDING_HANDTEKENING;
		}
		afmelding.setAfmeldingAanvraag(briefService.maakBvoBrief(afmelding, briefType, currentDateSupplier.getDate()));
		hibernateService.saveOrUpdate(afmelding);
	}

	@Override
	public void eenmaligAfmelden(MammaAfmelding afmelding, Account account)
	{
		var ronde = afmelding.getScreeningRonde();
		afspraakEnUitstelAfzeggen(ronde, account);
	}

	private void afspraakEnUitstelAfzeggen(MammaScreeningRonde ronde, Account account)
	{
		var annuleerMoment = millisecondenNaNu(100);
		var laatsteUitnodiging = ronde.getLaatsteUitnodiging();
		var laatsteAfspraak = laatsteUitnodiging != null ? ronde.getLaatsteUitnodiging().getLaatsteAfspraak() : null;

		if (laatsteAfspraak != null)
		{
			var status = account instanceof Client ? MammaAfspraakStatus.GEANNULEERD_CLIENT : MammaAfspraakStatus.GEANNULEERD_VIA_INFOLIJN;
			baseAfspraakService.afspraakAnnuleren(laatsteAfspraak, status, annuleerMoment);
			LOG.info("Afmelding: laatste afspraak (id: '{}') is afgezegd", laatsteAfspraak.getId());
		}

		if (ronde.getLaatsteUitstel() != null)
		{
			baseUitstelService.uitstelAfzeggen(ronde.getLaatsteUitstel(), MammaUitstelGeannuleerdReden.AFMELDING, annuleerMoment);
		}
	}

	@Override
	public void vervolgAfmelden(MammaAfmelding afmelding)
	{
		if (afmelding.getType() == DEFINITIEF)
		{
			afmelding.setAfmeldingBevestiging(
				briefService.maakBvoBrief(afmelding, BriefType.MAMMA_BEVESTIGING_DEFINITIEVE_AFMELDING, currentDateSupplier.getDate()));
			hibernateService.saveOrUpdate(afmelding);
		}
	}

	@Override
	public void vervolgHeraanmelden(MammaAfmelding herAanTeMeldenAfmelding, Account account)
	{
		if (herAanTeMeldenAfmelding.getType() == DEFINITIEF)
		{
			herAanTeMeldenAfmelding.setHeraanmeldBevestiging(
				briefService.maakBvoBrief(herAanTeMeldenAfmelding, BriefType.MAMMA_HERAANMELDING_BEVESTIGING, millisecondenNaNu(200)));
			hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);

			if (dossierService.isAutomatischRondeForcerenNaHeraanmeldenMogelijk(herAanTeMeldenAfmelding.getDossier()))
			{
				dossierService.rondeForceren(herAanTeMeldenAfmelding.getDossier().getClient());
			}
		}
	}

	private Date millisecondenNaNu(int miliseconden)
	{
		return DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().plus(miliseconden, ChronoUnit.MILLIS));
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public MammaScreeningRonde getGeldigeRondeVoorHeraanmelding(MammaAfmelding herAanTeMeldenAfmelding)
	{
		if (!DEFINITIEF.equals(herAanTeMeldenAfmelding.getType()))
		{
			throw new IllegalStateException("Geldige ronde alleen ondersteund bij definitieve heraanmelding voor BK");
		}

		var laatsteRonde = herAanTeMeldenAfmelding.getDossier().getLaatsteScreeningRonde();

		if (magLaatsteRondeHeropenen(laatsteRonde))
		{
			return laatsteRonde;
		}
		return null;
	}

	private boolean magLaatsteRondeHeropenen(MammaScreeningRonde laatsteRonde)
	{
		return laatsteRonde != null && screeningRondeService.isRondeNogGeldig(laatsteRonde) && !heeftOfKrijgtUitslag(laatsteRonde);
	}

	private boolean heeftOfKrijgtUitslag(MammaScreeningRonde ronde)
	{
		return ronde.getLaatsteOnderzoek() != null && ronde.getLaatsteOnderzoek().getLaatsteBeoordeling() != null;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean magAfmelden(Client client, boolean viaClientPortaal)
	{
		return isDefinitiefAfmeldenMogelijk(client, viaClientPortaal) || isEenmaligAfmeldenMogelijk(client);
	}

	private boolean isDefinitiefAfmeldenMogelijk(Client client, boolean viaClientportaal)
	{
		var dossier = client.getMammaDossier();
		var laatsteDefinitieveAfmelding = dossier.getLaatsteAfmelding();
		var ronde = dossier.getLaatsteScreeningRonde();
		var kanDefinitiefAfmelden = laatsteDefinitieveAfmelding == null || laatsteDefinitieveAfmelding.getHeraanmeldStatus() == AanvraagBriefStatus.VERWERKT
			|| viaClientportaal;
		if (ronde != null)
		{
			kanDefinitiefAfmelden &= rondeHeeftGeenActiefOnderzoek(ronde);
		}
		return kanDefinitiefAfmelden;
	}

	private boolean isEenmaligAfmeldenMogelijk(Client client)
	{
		var dossier = client.getMammaDossier();
		var ronde = dossier.getLaatsteScreeningRonde();
		if (ronde != null && ronde.getStatus() == ScreeningRondeStatus.LOPEND)
		{
			return rondeHeeftGeenActiefOnderzoek(ronde);
		}
		return false;
	}

	@Override
	public List<AfmeldingType> getBeschikbareAfmeldopties(Client client, boolean viaClientportaal)
	{
		var afmeldingTypes = new ArrayList<AfmeldingType>();
		if (isEenmaligAfmeldenMogelijk(client))
		{
			afmeldingTypes.add(AfmeldingType.EENMALIG);
		}
		if (isDefinitiefAfmeldenMogelijk(client, viaClientportaal))
		{
			afmeldingTypes.add(DEFINITIEF);
		}
		return afmeldingTypes;
	}

	private boolean rondeHeeftGeenActiefOnderzoek(MammaScreeningRonde ronde)
	{
		var afspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(ronde);
		return (afspraak == null || !MammaAfspraakStatus.isGestart(afspraak.getStatus())
			|| (afspraak.getOnderzoek() != null && MammaOnderzoekStatus.ONDERBROKEN_ZONDER_VERVOLG.equals(afspraak.getOnderzoek().getStatus()))
			|| screeningRondeService.heeftGeprinteOfTegengehoudenUitslagBrief(ronde)) && !isLaatsteOnderzoekOnderbrokenMetBeelden(ronde);
	}

	private boolean isLaatsteOnderzoekOnderbrokenMetBeelden(MammaScreeningRonde ronde)
	{
		var laatsteOnderzoek = ronde.getLaatsteOnderzoek();
		return laatsteOnderzoek != null && laatsteOnderzoek.getStatus().equals(MammaOnderzoekStatus.ONDERBROKEN)
			&& MammaMammografieIlmStatus.beeldenBeschikbaarOfBeschikbaarGeweest(laatsteOnderzoek.getMammografie().getIlmStatus());
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean magHeraanmelden(Client client)
	{
		return magDefinitiefHeraanmelden(client) || magEenmaligHeraanmelden(client);
	}

	private boolean magDefinitiefHeraanmelden(Client client)
	{
		var laatsteDefinitieveAfmelding = client.getMammaDossier().getLaatsteAfmelding();
		return laatsteDefinitieveAfmelding != null && laatsteDefinitieveAfmelding.getAfmeldingStatus() == AanvraagBriefStatus.VERWERKT
			&& laatsteDefinitieveAfmelding.getHeraanmeldStatus() != AanvraagBriefStatus.VERWERKT;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean magEenmaligHeraanmelden(Client client)
	{
		var dossier = client.getMammaDossier();
		var laatsteRonde = dossier.getLaatsteScreeningRonde();
		if (magLaatsteRondeHeropenen(laatsteRonde))
		{
			return !laatsteRonde.getAangemeld() && dossier.getAangemeld();
		}
		return false;
	}

	@Override
	public String getAanvullendeHeraanmeldLogMelding(MammaAfmelding definitieveAfmelding)
	{
		return definitieveAfmelding.getDossier() != null && definitieveAfmelding.getDossier().getLaatsteScreeningRonde() != null
			&& definitieveAfmelding.getDossier().getLaatsteScreeningRonde().isGeforceerd() ? ". Ronde geforceerd" : "";
	}
}
