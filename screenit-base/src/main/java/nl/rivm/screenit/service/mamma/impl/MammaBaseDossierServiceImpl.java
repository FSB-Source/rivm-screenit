package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.Comparator;
import java.util.stream.Stream;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaKansberekeningScreeningRondeEvent;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.service.BaseClientContactService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaAfmeldService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseDossierServiceImpl implements MammaBaseDossierService
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	@Lazy
	private ClientService clientService;

	@Autowired
	private BaseClientContactService baseClientContactService;

	@Autowired
	@Lazy
	private MammaBaseScreeningrondeService mammaBaseScreeningrondeService;

	@Autowired
	private MammaBaseFollowUpService followUpService;

	@Autowired(required = false)
	private MammaAfmeldService afmeldService;

	@Override
	public MammaFactorType getFactorType(MammaDossier dossier)
	{
		return MammaFactorType.getFactorType(dossier.getTehuis() != null, dossier.getDoelgroep(), dossier.getLaatsteMammografieAfgerond());
	}

	@Override
	public long aantalOproepen(MammaDossier dossier)
	{
		return dossier.getScreeningRondes().size();
	}

	@Override
	public long aantalOpgekomenSE(MammaDossier dossier)
	{
		return dossier.getScreeningRondes().stream()
			.filter(sr -> heeftOnderzoek(sr) && !sr.getId().equals(dossier.getLaatsteScreeningRonde().getId())).count();
	}

	@Override
	public long aantalOpgekomenBE(MammaDossier dossier)
	{
		return dossier.getScreeningRondes().stream().filter(this::heeftOnderzoek).count();
	}

	@Override
	public Stream<MammaScreeningRonde> laatste3AfgerondeRondesMetOnderzoek(MammaDossier dossier)
	{
		return dossier.getScreeningRondes().stream().filter(r -> r.getStatus() == ScreeningRondeStatus.AFGEROND).filter(this::heeftOnderzoek)
			.sorted((r1, r2) -> r2.getStatusDatum().compareTo(r1.getStatusDatum())).limit(3);
	}

	private boolean heeftOnderzoek(MammaScreeningRonde ronde)
	{
		return ronde.getLaatsteOnderzoek() != null;
	}

	@Override
	public boolean isAfspraakMakenMogelijk(MammaDossier dossier, boolean viaClientportaal, boolean viaSePassant)
	{
		if (heeftGbaPostcode(dossier))
		{
			MammaScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			if (ronde != null && !ronde.getMinderValideOnderzoekZiekenhuis()
				&& (ronde.getStatus() == ScreeningRondeStatus.LOPEND || viaSePassant && afmeldService.magEenmaligHeraanmelden(dossier.getClient())))
			{
				MammaUitnodiging laatsteUitnodiging = ronde.getLaatsteUitnodiging();
				if (laatsteUitnodiging != null)
				{
					MammaAfspraak laatsteAfspraak = laatsteUitnodiging.getLaatsteAfspraak();
					MammaOnderzoek onderzoekVanLaatsteAfspraak = null;
					if (laatsteAfspraak != null)
					{
						onderzoekVanLaatsteAfspraak = laatsteAfspraak.getOnderzoek();
					}
					boolean isOnderzoekVanLaatsteAfspraakOnderbrokenOfOpgeschort = onderzoekVanLaatsteAfspraak != null && onderzoekVanLaatsteAfspraak.isDoorgevoerd()
						&& (onderzoekVanLaatsteAfspraak.getStatus() == MammaOnderzoekStatus.ONDERBROKEN
							|| onderzoekVanLaatsteAfspraak.getStatus() == MammaOnderzoekStatus.ONDERBROKEN_ZONDER_VERVOLG
							|| MammaBeoordelingStatus.OPGESCHORT.equals(onderzoekVanLaatsteAfspraak.getLaatsteBeoordeling().getStatus()) && !viaClientportaal);

					boolean isLaatsteAfspraakNoShow = laatsteAfspraak != null && laatsteAfspraak.getStatus() == MammaAfspraakStatus.GEPLAND
						&& laatsteAfspraak.getVanaf().compareTo(currentDateSupplier.getDate()) < 0;

					return !isOnvolledigOnderzoek(ronde.getLaatsteOnderzoek()) &&
						(laatsteAfspraak == null
							|| MammaAfspraakStatus.isGeannuleerd(laatsteAfspraak.getStatus())
							|| isLaatsteAfspraakNoShow
							|| isOnderzoekVanLaatsteAfspraakOnderbrokenOfOpgeschort);
				}
			}
		}
		return false;
	}

	private boolean isOnvolledigOnderzoek(MammaOnderzoek onderzoek)
	{
		return onderzoek != null && onderzoek.isDoorgevoerd() && onderzoek.getStatus().equals(MammaOnderzoekStatus.ONVOLLEDIG);
	}

	@Override
	public boolean isVerzettenMogelijk(MammaDossier dossier)
	{
		if (heeftGbaPostcode(dossier))
		{
			MammaScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			if (ronde != null)
			{
				if (ronde.getStatus() == ScreeningRondeStatus.LOPEND)
				{
					MammaUitnodiging laatsteUitnodiging = ronde.getLaatsteUitnodiging();
					if (laatsteUitnodiging != null)
					{
						MammaAfspraak laatsteAfspraak = laatsteUitnodiging.getLaatsteAfspraak();
						if (laatsteAfspraak != null && !isAfspraakMakenMogelijk(dossier, false, false))
						{
							return laatsteAfspraak.getStatus() == MammaAfspraakStatus.GEPLAND && !isOnvolledigOnderzoek(ronde.getLaatsteOnderzoek());
						}
					}
				}
			}
		}
		return false;
	}

	@Override
	public MammaOnderzoek getLaatsteOnderzoek(MammaDossier dossier)
	{
		return dossier.getScreeningRondes().stream()
			.flatMap(screeningRonde -> screeningRonde.getUitnodigingen().stream())
			.flatMap(uitnodiging -> uitnodiging.getAfspraken().stream())
			.filter(afspraak -> afspraak.getOnderzoek() != null)
			.map(MammaAfspraak::getOnderzoek).max(Comparator.comparing(MammaOnderzoek::getCreatieDatum)).orElse(null);
	}

	@Override
	public boolean isSuspect(MammaDossier dossier)
	{
		MammaScreeningRonde laatsteScreeningRondeMetAfspraak = dossier.getScreeningRondes().stream().filter(screeningRonde -> {
			MammaUitnodiging uitnodiging = screeningRonde.getLaatsteUitnodiging();
			MammaAfspraak afspraak = uitnodiging != null ? uitnodiging.getLaatsteAfspraak() : null;
			return afspraak != null;
		}).max(Comparator.comparing(MammaScreeningRonde::getCreatieDatum)).orElse(null);

		if (laatsteScreeningRondeMetAfspraak != null)
		{
			MammaAfspraak laatsteAfspraak = laatsteScreeningRondeMetAfspraak.getLaatsteUitnodiging().getLaatsteAfspraak();
			return isSuspect(laatsteScreeningRondeMetAfspraak, laatsteAfspraak);
		}

		return false;
	}

	private boolean isSuspect(MammaScreeningRonde laatsteScreeningRondeMetAfspraak, MammaAfspraak laatsteAfspraak)
	{
		MammaOnderzoek onderzoek = laatsteAfspraak.getOnderzoek();
		MammaBeoordeling beoordeling = onderzoek != null ? onderzoek.getLaatsteBeoordeling() : null;
		MammaFollowUpConclusieStatus followUpConclusieStatus = laatsteScreeningRondeMetAfspraak.getFollowUpConclusieStatus();
		if (beoordeling != null && MammaBeoordelingStatus.UITSLAG_ONGUNSTIG.equals(beoordeling.getStatus()))
		{

			return followUpConclusieStatus == null || followUpConclusieStatus.equals(MammaFollowUpConclusieStatus.FALSE_NEGATIVE)
				|| followUpConclusieStatus.equals(MammaFollowUpConclusieStatus.TRUE_POSITIVE);
		}
		else if (beoordeling != null
			&& (MammaBeoordelingStatus.UITSLAG_GUNSTIG.equals(beoordeling.getStatus()) || MammaBeoordelingStatus.ONBEOORDEELBAAR.equals(beoordeling.getStatus())))
		{
			return followUpConclusieStatus != null && followUpConclusieStatus.equals(MammaFollowUpConclusieStatus.FALSE_NEGATIVE);
		}
		return false;
	}

	@Override
	public boolean isSuspect(MammaKansberekeningScreeningRondeContext kansberekeningScreeningRondeContext)
	{
		if (!kansberekeningScreeningRondeContext.afspraakNavigableMap.isEmpty())
		{
			MammaAfspraak afspraak = kansberekeningScreeningRondeContext.afspraakNavigableMap.lastEntry().getValue();

			return isSuspect(afspraak.getUitnodiging().getScreeningRonde(), afspraak);
		}
		return false;
	}

	@Override
	public boolean isAfspraakForcerenMogelijk(MammaDossier dossier)
	{
		boolean magAfspraakForceren = false;
		if (dossier.getLaatsteScreeningRonde() != null)
		{
			MammaOnderzoek onderzoek = dossier.getLaatsteScreeningRonde().getLaatsteOnderzoek();

			if (onderzoek != null && onderzoek.isDoorgevoerd() && onderzoek.getMammografie() != null)
			{
				int MAX_DAGEN_UITSTELLEN_FORCEREN = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_UITNODIGINGEN.name());
				LocalDate afspraakDatum180Dagen = DateUtil.toLocalDate(onderzoek.getMammografie().getAfgerondOp()).plusDays(MAX_DAGEN_UITSTELLEN_FORCEREN);
				magAfspraakForceren = currentDateSupplier.getLocalDate().isBefore(afspraakDatum180Dagen);
			}
		}
		return magAfspraakForceren;
	}

	@Override
	public boolean isRondeForcerenMogelijk(MammaDossier dossier)
	{
		if (dossier.getStatus() != DossierStatus.ACTIEF || dossier.getTehuis() != null || !heeftGbaPostcode(dossier))
		{
			return false;
		}

		MammaScreeningRonde laatsteRonde = dossier.getLaatsteScreeningRonde();
		if (laatsteRonde == null)
		{
			return true;
		}
		return laatsteRonde.getStatus() == ScreeningRondeStatus.AFGEROND || laatsteRonde.getLaatsteUitnodiging() == null;

	}

	private boolean heeftGbaPostcode(MammaDossier dossier)
	{
		return StringUtils.isNotBlank(clientService.getGbaPostcode(dossier.getClient()));
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderMammaDossier(Client client)
	{
		baseClientContactService.verwijderClientContacten(client, Bevolkingsonderzoek.MAMMA);

		MammaDossier dossier = client.getMammaDossier();
		if (dossier != null)
		{
			mammaBaseScreeningrondeService.verwijderAlleScreeningRondes(dossier);

			dossier.setInactiefVanaf(null);
			dossier.setInactiefTotMet(null);

			if (DossierStatus.INACTIEF.equals(dossier.getStatus()) && Boolean.TRUE.equals(dossier.getAangemeld()))
			{
				dossier.setStatus(DossierStatus.ACTIEF);
			}

			MammaKansberekeningScreeningRondeEvent screeningRondeEvent = dossier.getScreeningRondeEvent();
			if (screeningRondeEvent != null)
			{
				dossier.setScreeningRondeEvent(null);
				hibernateService.delete(screeningRondeEvent);
			}

			hibernateService.saveOrUpdate(dossier);
			followUpService.refreshUpdateFollowUpConclusie(dossier);
		}

		hibernateService.saveOrUpdate(client);

		ProjectClient projectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate(), false);
		if (projectClient != null)
		{
			clientService.projectClientInactiveren(projectClient, ProjectInactiefReden.VERWIJDERING_VAN_DOSSIER, null);
		}
	}
}
