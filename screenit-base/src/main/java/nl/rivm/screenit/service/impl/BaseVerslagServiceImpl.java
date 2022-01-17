package nl.rivm.screenit.service.impl;

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

import java.util.Date;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.mamma.MammaBaseBeoordelingDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.BaseScreeningRondeService;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.VerwerkVerslagService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
public class BaseVerslagServiceImpl implements BaseVerslagService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired(required = false)
	private MammaBaseFollowUpService followUpService;

	@Autowired
	private LogService logService;

	@Autowired(required = false)
	private ColonDossierBaseService colonDossierService;

	@Autowired
	private VerwerkVerslagService verwerkVerslagService;

	@Autowired(required = false)
	private MammaBaseBeoordelingDao mammaBeoordelingDao;

	@Autowired
	private BaseScreeningRondeService baseScreeningRondeService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
	public void verwijderVerslag(Verslag verslag, InstellingGebruiker instellingGebruiker, boolean heropenRondeEnDossier)
	{
		VerslagType type = verslag.getType();
		Class<? extends Verslag<?, ?>> verslagClazz = type.getClazz();
		verslag = hibernateService.load(verslagClazz, verslag.getId());

		if (heropenRondeEnDossier)
		{
			heropenRondeEnDossier(verslag);
		}

		String melding = createLogMelding(verslag);
		ScreeningRonde screeningRonde = verslag.getScreeningRonde();
		Client client = screeningRonde.getDossier().getClient();

		if (instellingGebruiker != null)
		{
			logService.logGebeurtenis(type.getVerwijderdVerslagLogGebeurtenis(), instellingGebruiker, client, melding, type.getBevolkingsonderzoek());
		}
		else
		{
			logService.logGebeurtenis(type.getVerwijderdVerslagLogGebeurtenis(), client, melding, type.getBevolkingsonderzoek());
		}

		removeVerslagUitRonde(verslag, type, client);
		verslag.setScreeningRonde(null);
		verslag.setUitvoerderMedewerker(null);
		verslag.setUitvoerderOrganisatie(null);
		verslag.setOntvangenBericht(null);

		OntvangenCdaBericht ontvangenBericht = verslag.getOntvangenBericht();
		if (ontvangenBericht != null)
		{
			ontvangenBericht.setStatus(BerichtStatus.VERWIJDERD);
			hibernateService.saveOrUpdate(ontvangenBericht);
		}

		hibernateService.saveOrUpdate(screeningRonde);
		hibernateService.delete(verslag);
		hibernateService.delete(verslag.getVerslagContent());

	}

	private void removeVerslagUitRonde(Verslag verslag, VerslagType type, Client client)
	{
		if (type == VerslagType.MDL && VerslagStatus.AFGEROND == verslag.getStatus())
		{
			MdlVerslag mdlVerslag = (MdlVerslag) HibernateHelper.deproxy(verslag);
			ColonScreeningRonde colonScreeningRonde = mdlVerslag.getScreeningRonde();
			colonScreeningRonde.getVerslagen().remove(mdlVerslag);
		}
		else if (type == VerslagType.PA_LAB)
		{
			PaVerslag paVerslag = (PaVerslag) HibernateHelper.deproxy(verslag);
			ColonScreeningRonde colonScreeningRonde = paVerslag.getScreeningRonde();
			colonScreeningRonde.getVerslagen().remove(paVerslag);
		}
		else if (type == VerslagType.MAMMA_PA_FOLLOW_UP)
		{
			MammaFollowUpVerslag followupVerslag = (MammaFollowUpVerslag) HibernateHelper.deproxy(verslag);
			MammaScreeningRonde mammaScreeningRonde = followupVerslag.getScreeningRonde();
			mammaScreeningRonde.getFollowUpVerslagen().remove(followupVerslag);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void heropenRondeEnDossier(Verslag verslag)
	{
		VerslagType type = verslag.getType();
		if (type == VerslagType.MDL && VerslagStatus.AFGEROND == verslag.getStatus())
		{
			MdlVerslag mdlVerslag = (MdlVerslag) HibernateHelper.deproxy(verslag);
			MdlVervolgbeleid vervolgbeleidHuidigVerslag = mdlVerslag.getVervolgbeleid();
			MdlVerslag nieuweLaatsteAfgerondVerslag = null;
			ColonScreeningRonde screeningRonde = mdlVerslag.getScreeningRonde();
			boolean geenAnderVerslagMetDefinitiefVervolgbeleid = true;
			for (ColonVerslag<?> oneOfAllVerslagen : screeningRonde.getVerslagen())
			{
				if (VerslagType.MDL == oneOfAllVerslagen.getType() && VerslagStatus.AFGEROND == oneOfAllVerslagen.getStatus() && !oneOfAllVerslagen.equals(mdlVerslag))
				{
					MdlVerslag anderMdlVerslag = (MdlVerslag) HibernateHelper.deproxy(oneOfAllVerslagen);
					if (MdlVervolgbeleid.isDefinitief(anderMdlVerslag.getVervolgbeleid()))
					{
						geenAnderVerslagMetDefinitiefVervolgbeleid = false;
					}
					if (nieuweLaatsteAfgerondVerslag == null || DateUtil.compareAfter(nieuweLaatsteAfgerondVerslag.getDatumOnderzoek(), anderMdlVerslag.getDatumOnderzoek()))
					{
						nieuweLaatsteAfgerondVerslag = anderMdlVerslag;
					}
				}
			}
			ColonDossier dossier = screeningRonde.getDossier();
			if (MdlVervolgbeleid.isDefinitief(vervolgbeleidHuidigVerslag) && geenAnderVerslagMetDefinitiefVervolgbeleid)
			{

				if (ScreeningRondeStatus.AFGEROND == screeningRonde.getStatus())
				{
					screeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
					screeningRonde.setStatusDatum(currentDateSupplier.getDate());
					screeningRonde.setAfgerondReden(null);
					hibernateService.saveOrUpdate(screeningRonde);

					if (DossierStatus.INACTIEF == dossier.getStatus())
					{
						dossier.setStatus(DossierStatus.ACTIEF);
						dossier.setInactiveerReden(null);
						dossier.setInactiefVanaf(null);
						dossier.setInactiefTotMet(null);
						hibernateService.saveOrUpdate(dossier);
					}
				}
			}
			if (nieuweLaatsteAfgerondVerslag != null)
			{
				colonDossierService.setVolgendeUitnodigingVoorVerslag(nieuweLaatsteAfgerondVerslag);
			}
			else
			{
				ColonIntakeAfspraak laatsteAfspraak = screeningRonde.getLaatsteAfspraak();
				if (laatsteAfspraak != null)
				{
					colonDossierService.setVolgendeUitnodingVoorConclusie(laatsteAfspraak);
				}
			}
			verwerkVerslagService.ontkoppelOfVerwijderComplicaties(mdlVerslag);
		}

		else if (type == VerslagType.MAMMA_PA_FOLLOW_UP)
		{
			MammaFollowUpVerslag followupVerslag = (MammaFollowUpVerslag) HibernateHelper.deproxy(verslag);
			followUpService.refreshUpdateFollowUpConclusie(followupVerslag.getScreeningRonde().getDossier());
			MammaScreeningRonde screeningRonde = followupVerslag.getScreeningRonde();
			MammaScreeningRonde screeningrondeVoorFollowUp = mammaBeoordelingDao.getScreeningrondeVoorFollowUp(screeningRonde.getDossier().getClient(), null);
			if (screeningRonde.equals(screeningrondeVoorFollowUp))
			{
				screeningRonde.setFollowUpConclusieStatus(null);
				screeningRonde.setFollowUpConclusieStatusGewijzigdOp(null);
				hibernateService.saveOrUpdate(screeningRonde);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public String createLogMelding(Verslag verslag)
	{
		String melding = "";
		OntvangenCdaBericht ontvangenBericht = verslag.getOntvangenBericht();
		if (ontvangenBericht != null)
		{
			melding = "Electronisch bericht: berichtId: " + ontvangenBericht.getBerichtId() + ", setId: " + ontvangenBericht.getSetId() + ", versie: "
				+ ontvangenBericht.getVersie() + ",";
		}
		else
		{
			melding = "Handmatige invoer: ";
		}
		Date datumOnderzoek = verslag.getDatumOnderzoek();
		if (datumOnderzoek != null)
		{
			melding += " datum onderzoek " + Constants.getDateFormat().format(datumOnderzoek);
		}
		if (verslag.getId() == null)
		{
			melding = "Nieuw. " + melding;
		}
		return melding;
	}
}
