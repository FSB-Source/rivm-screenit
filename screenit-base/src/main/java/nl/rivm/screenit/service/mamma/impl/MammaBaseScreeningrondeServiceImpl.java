package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.mamma.MammaBaseScreeningrondeDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaKansberekeningAfspraakEvent;
import nl.rivm.screenit.model.mamma.MammaKansberekeningScreeningRondeEvent;
import nl.rivm.screenit.model.mamma.MammaOpkomstkans;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseIlmService;
import nl.rivm.screenit.service.mamma.MammaBaseKwaliteitscontroleService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.service.mamma.MammaBaseUitwisselportaalService;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaBaseScreeningrondeServiceImpl implements MammaBaseScreeningrondeService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private MammaBaseScreeningrondeDao baseScreeningrondeDao;

	@Autowired
	private MammaBaseBeoordelingService baseBeoordelingService;

	@Autowired
	private MammaBaseUitwisselportaalService baseUitwisselportaalService;

	@Autowired
	private MammaBaseKwaliteitscontroleService baseKwaliteitscontroleService;

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaBaseIlmService baseIlmService;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean heeftGeprinteOfTegengehoudenUitslagBrief(MammaScreeningRonde screeningRonde)
	{
		return screeningRonde.getBrieven().stream().anyMatch(
			brief -> BriefType.isMammaUitslagBrief(brief.getBriefType()) &&
				(BriefUtil.isMergedBrievenGeprint(brief)
					|| BriefUtil.getMergedBrieven(brief) == null && BriefUtil.isGegenereerd(brief)
					|| BriefUtil.isTegengehouden(brief)));
	}

	@Override
	public void verwijderAlleScreeningRondes(MammaDossier dossier)
	{
		List<MammaScreeningRonde> rondes = dossier.getScreeningRondes();

		dossier.setLaatsteScreeningRonde(null);
		dossier.setScreeningRondes(new ArrayList<>());
		hibernateService.saveOrUpdate(dossier);
		if (CollectionUtils.isNotEmpty(rondes))
		{
			for (MammaScreeningRonde ronde : rondes)
			{
				verwijderScreeningRonde(ronde, true);
			}
		}
	}

	@Override
	public boolean verwijderScreeningRonde(MammaScreeningRonde screeningRonde, boolean forceerBeeldenVerwijderen)
	{
		if (heeftIlmBeelden(screeningRonde))
		{
			if (forceerBeeldenVerwijderen)
			{
				berichtToBatchService.queueMammaHL7v24BerichtUitgaand(screeningRonde, MammaHL7v24ORMBerichtStatus.GOINGTODELETE);
				berichtToBatchService.queueMammaHL7v24BerichtUitgaand(screeningRonde, MammaHL7v24ORMBerichtStatus.DELETE);
				baseIlmService.maakIlmBezwaarPoging(screeningRonde.getDossier(), screeningRonde.getUitnodigingsNr(), false);
			}
			else
			{
				return false;
			}
		}
		if (forceerBeeldenVerwijderen)
		{
			baseUitwisselportaalService.verwijderUploadVerzoeken(screeningRonde);
		}
		baseUitwisselportaalService.verwijderDownloadVerzoeken(screeningRonde);
		baseKwaliteitscontroleService.verwijderKwaliteitscontroleOnderzoeken(screeningRonde);

		MammaDossier dossier = screeningRonde.getDossier();
		boolean isRondeMetLaatsteBeoordelingMetUitslag = dossier.getLaatsteBeoordelingMetUitslag() != null &&
			dossier.getLaatsteBeoordelingMetUitslag().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().equals(screeningRonde);
		if (isRondeMetLaatsteBeoordelingMetUitslag)
		{
			dossier.setLaatsteBeoordelingMetUitslag(null);
		}
		screeningRonde.setLaatsteOnderzoek(null);
		screeningRonde.setLaatsteUitnodiging(null);
		screeningRonde.setLaatsteBrief(null);
		screeningRonde.setLaatsteUitstel(null);
		screeningRonde.setLaatsteAfmelding(null);
		hibernateService.deleteAll(screeningRonde.getUitstellen());
		verwijderAlleUitnodigingen(screeningRonde.getUitnodigingen());
		hibernateService.deleteAll(screeningRonde.getBrieven());
		hibernateService.deleteAll(screeningRonde.getAfmeldingen());
		hibernateService.deleteAll(screeningRonde.getFollowUpVerslagen());
		hibernateService.deleteAll(screeningRonde.getFollowUpRadiologieVerslagen());

		MammaKansberekeningScreeningRondeEvent screeningRondeEvent = screeningRonde.getScreeningRondeEvent();
		if (screeningRondeEvent != null)
		{
			screeningRonde.setScreeningRondeEvent(null);
			hibernateService.delete(screeningRondeEvent);
		}

		dossier.getScreeningRondes().stream().filter(screeningRonde::equals).findFirst()
			.ifPresent(screeningRondeToDelete -> screeningRonde.getDossier().getScreeningRondes().remove(screeningRondeToDelete));

		if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().equals(screeningRonde))
		{
			dossier.setLaatsteScreeningRonde(dossier.getScreeningRondes().stream()
				.min((screeningRonde1, screeningRonde2) -> screeningRonde1.getCreatieDatum().compareTo(screeningRonde2.getCreatieDatum()) * -1).orElse(null));
		}

		if (isRondeMetLaatsteBeoordelingMetUitslag)
		{
			dossier.setLaatsteBeoordelingMetUitslag(baseBeoordelingService.getLaatsteBeoordelingMetUitslag(dossier));
		}

		screeningRonde.setDossier(null);
		hibernateService.delete(screeningRonde);
		hibernateService.saveOrUpdate(dossier);
		return true;
	}

	private boolean heeftIlmBeelden(MammaScreeningRonde ronde)
	{
		return ronde.getUitnodigingen().stream().flatMap(u -> u.getAfspraken().stream())
			.anyMatch(a -> a.getOnderzoek() != null && a.getOnderzoek().getMammografie() != null
				&& MammaMammografieIlmStatus.beeldenMogelijkAanwezig(a.getOnderzoek().getMammografie().getIlmStatus()));
	}

	private void verwijderAlleUitnodigingen(List<MammaUitnodiging> uitnodigingen)
	{
		uitnodigingen.forEach(uitnodiging ->
		{
			uitnodiging.getAfspraken().forEach(afspraak ->
			{
				if (afspraak.getOnderzoek() != null)
				{
					afspraak.getOnderzoek().getBeoordelingen()
						.forEach(beoordeling ->
						{
							hibernateService.deleteAll(beoordeling.getHuisartsBerichten());
							if (beoordeling.getVerslagPdf() != null)
							{
								uploadDocumentService.delete(beoordeling.getVerslagPdf(), true);
							}
						});
					hibernateService.deleteAll(afspraak.getOnderzoek().getBeoordelingen());
					hibernateService.delete(afspraak.getOnderzoek());
				}
				MammaOpkomstkans opkomstkans = afspraak.getOpkomstkans();
				if (opkomstkans != null)
				{
					hibernateService.delete(opkomstkans);
				}
				MammaKansberekeningAfspraakEvent afspraakEvent = afspraak.getAfspraakEvent();
				if (afspraakEvent != null)
				{
					hibernateService.delete(afspraakEvent);
				}
				hibernateService.delete(afspraak);
			});

			MammaBrief uitnodigingBrief = uitnodiging.getBrief();
			if (uitnodigingBrief != null)
			{
				hibernateService.delete(uitnodigingBrief);
			}
			hibernateService.delete(uitnodiging);
		});
	}

	@Override
	public MammaScreeningRonde getRondeVanUitnodigingsr(Client client, Long uitnodigingsNr)
	{
		return client.getMammaDossier().getScreeningRondes().stream()
			.filter(sr -> uitnodigingsNr.equals(sr.getUitnodigingsNr()))
			.findFirst().orElse(null);
	}

	@Override
	public MammaScreeningRonde getLaatsteScreeningRondeMetUitslag(Client client)
	{
		return baseScreeningrondeDao.getLaatsteScreeningRondeMetUitslag(client, null);
	}

	@Override
	public MammaScreeningRonde getLaatsteScreeningRondeMetUitslag(Client client, Date voorDatum)
	{
		return baseScreeningrondeDao.getLaatsteScreeningRondeMetUitslag(client, voorDatum);
	}

	@Override
	public MammaScreeningRonde getLaatsteScreeningRondeMetPositieveUitslag(Client client)
	{
		return baseScreeningrondeDao.getLaatsteScreeningRondeMetPositieveUitslag(client, null);
	}

	@Override
	public Integer getJaarLaatsteVerwijzing(Client client)
	{
		MammaScreeningRonde laatsteScreeningRondeMetPositieveUitslag = getLaatsteScreeningRondeMetPositieveUitslag(client);
		return laatsteScreeningRondeMetPositieveUitslag != null
			? DateUtil.toLocalDate(laatsteScreeningRondeMetPositieveUitslag.getLaatsteOnderzoek().getCreatieDatum()).getYear()
			: null;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public BriefType bepaalBriefTypeVoorOpenUitnodiging(boolean isSuspect, MammaDoelgroep doelgroep)
	{
		if (isSuspect)
		{
			return BriefType.MAMMA_UITNODIGING_SUSPECT;
		}
		else if (MammaDoelgroep.MINDER_VALIDE.equals(doelgroep))
		{
			return BriefType.MAMMA_UITNODIGING_MINDER_VALIDE;
		}
		else
		{
			return BriefType.MAMMA_OPEN_UITNODIGING;
		}
	}

	@Override
	public boolean isRondeNogGeldig(MammaScreeningRonde ronde)
	{
		var creatieDatum = DateUtil.toLocalDate(ronde.getCreatieDatum());
		var minimaleCreatieDatum = currentDateSupplier.getLocalDate().minusMonths(Constants.BK_GELDIGHEID_RONDE_MAANDEN);
		return !creatieDatum.isBefore(minimaleCreatieDatum);
	}
}
