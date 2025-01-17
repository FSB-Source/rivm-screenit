package nl.rivm.screenit.service.impl;

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
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht_;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlTnummerPathologieVerslag;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.repository.algemeen.DSValueRepository;
import nl.rivm.screenit.repository.colon.ColonMdlVerslagRepository;
import nl.rivm.screenit.repository.colon.ColonPaVerslagRepository;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftNietId;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftCode;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftCodeSystem;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftDatumOnderzoek;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftScreeningRondeInMdlVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftScreeningRondeInPaVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftTNummerIn;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftTNummerInPaVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftValueSetName;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftVerslagStatus;

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

	@Autowired(required = false)
	private MammaBaseScreeningrondeService baseScreeningrondeService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ColonPaVerslagRepository paVerslagRepository;

	@Autowired
	private DSValueRepository dsValueRepository;

	@Autowired
	private ColonMdlVerslagRepository mdlVerslagRepository;

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
	public void verwijderVerslag(Verslag<?, ?> verslag, InstellingGebruiker instellingGebruiker, boolean heropenRondeEnDossier)
	{
		var type = verslag.getType();
		var verslagClazz = type.getClazz();
		verslag = hibernateService.load(verslagClazz, verslag.getId());

		if (heropenRondeEnDossier)
		{
			heropenRondeEnDossier(verslag);
		}

		var melding = createLogMelding(verslag);
		var screeningRonde = verslag.getScreeningRonde();
		var client = screeningRonde.getDossier().getClient();

		if (instellingGebruiker != null)
		{
			logService.logGebeurtenis(type.getVerwijderdVerslagLogGebeurtenis(), instellingGebruiker, client, melding, type.getBevolkingsonderzoek());
		}
		else
		{
			logService.logGebeurtenis(type.getVerwijderdVerslagLogGebeurtenis(), client, melding, type.getBevolkingsonderzoek());
		}

		removeVerslagUitRonde(verslag, type);
		verslag.setScreeningRonde(null);
		verslag.setUitvoerderMedewerker(null);
		verslag.setUitvoerderOrganisatie(null);
		verslag.setOntvangenBericht(null);

		var ontvangenBericht = verslag.getOntvangenBericht();
		if (ontvangenBericht != null)
		{
			ontvangenBericht.setStatus(BerichtStatus.VERWIJDERD);
			hibernateService.saveOrUpdate(ontvangenBericht);
		}

		hibernateService.saveOrUpdate(screeningRonde);
		hibernateService.delete(verslag);
		hibernateService.delete(verslag.getVerslagContent());

	}

	private void removeVerslagUitRonde(Verslag<?, ?> verslag, VerslagType type)
	{
		if (type == VerslagType.MDL && VerslagStatus.AFGEROND == verslag.getStatus())
		{
			var mdlVerslag = (MdlVerslag) HibernateHelper.deproxy(verslag);
			var colonScreeningRonde = mdlVerslag.getScreeningRonde();
			colonScreeningRonde.getVerslagen().remove(mdlVerslag);
		}
		else if (type == VerslagType.PA_LAB)
		{
			var paVerslag = (PaVerslag) HibernateHelper.deproxy(verslag);
			var colonScreeningRonde = paVerslag.getScreeningRonde();
			colonScreeningRonde.getVerslagen().remove(paVerslag);
		}
		else if (type == VerslagType.MAMMA_PA_FOLLOW_UP || type == VerslagType.MAMMA_PA_FOLLOW_UP_MONITOR)
		{
			var followupVerslag = (MammaFollowUpVerslag) HibernateHelper.deproxy(verslag);
			var mammaScreeningRonde = followupVerslag.getScreeningRonde();
			mammaScreeningRonde.getFollowUpVerslagen().remove(followupVerslag);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void heropenRondeEnDossier(Verslag<?, ?> verslag)
	{
		var type = verslag.getType();
		if (type == VerslagType.MDL && VerslagStatus.AFGEROND == verslag.getStatus())
		{
			var mdlVerslag = (MdlVerslag) HibernateHelper.deproxy(verslag);
			var vervolgbeleidHuidigVerslag = mdlVerslag.getVervolgbeleid();
			MdlVerslag nieuweLaatsteAfgerondVerslag = null;
			var screeningRonde = mdlVerslag.getScreeningRonde();
			var geenAnderVerslagMetDefinitiefVervolgbeleid = true;
			for (var oneOfAllVerslagen : screeningRonde.getVerslagen())
			{
				if (VerslagType.MDL == oneOfAllVerslagen.getType() && VerslagStatus.AFGEROND == oneOfAllVerslagen.getStatus() && !oneOfAllVerslagen.equals(mdlVerslag))
				{
					var anderMdlVerslag = (MdlVerslag) HibernateHelper.deproxy(oneOfAllVerslagen);
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
			var dossier = screeningRonde.getDossier();
			if (MdlVervolgbeleid.isDefinitief(vervolgbeleidHuidigVerslag) && geenAnderVerslagMetDefinitiefVervolgbeleid
				&& ScreeningRondeStatus.AFGEROND == screeningRonde.getStatus())
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

			if (nieuweLaatsteAfgerondVerslag != null)
			{
				colonDossierService.setVolgendeUitnodigingVoorVerslag(nieuweLaatsteAfgerondVerslag);
			}
			else
			{
				var laatsteAfspraak = screeningRonde.getLaatsteAfspraak();
				if (laatsteAfspraak != null)
				{
					colonDossierService.setVolgendeUitnodingVoorConclusie(laatsteAfspraak);
				}
			}
		}

		else if (type == VerslagType.MAMMA_PA_FOLLOW_UP)
		{
			var followupVerslag = (MammaFollowUpVerslag) HibernateHelper.deproxy(verslag);
			followUpService.refreshUpdateFollowUpConclusie(followupVerslag.getScreeningRonde().getDossier());
			var screeningRonde = followupVerslag.getScreeningRonde();
			var screeningrondeVoorFollowUp = baseScreeningrondeService.getLaatsteScreeningRondeMetUitslag(screeningRonde.getDossier().getClient());
			if (screeningRonde.equals(screeningrondeVoorFollowUp))
			{
				screeningRonde.setFollowUpConclusieStatus(null);
				screeningRonde.setFollowUpConclusieStatusGewijzigdOp(null);
				hibernateService.saveOrUpdate(screeningRonde);
			}
		}
	}

	@Override
	public String createLogMelding(Verslag<?, ?> verslag)
	{
		String melding;
		var verwerktVerslag = (Verslag<?, ?>) HibernateHelper.deproxy(verslag);
		var ontvangenBericht = verwerktVerslag.getOntvangenBericht();
		if (ontvangenBericht != null)
		{
			melding = "Elektronisch bericht: berichtId: " + ontvangenBericht.getBerichtId() + ", setId: " + ontvangenBericht.getSetId() + ", versie: "
				+ ontvangenBericht.getVersie() + ",";
		}
		else if (verwerktVerslag instanceof MammaFollowUpVerslag && isElektronischPalgaVerslag((MammaFollowUpVerslag) verwerktVerslag))
		{
			melding = "Elektronische invoer: ";
		}
		else
		{
			melding = "Handmatige invoer: ";
		}
		var datumOnderzoek = verwerktVerslag.getDatumOnderzoek();
		if (datumOnderzoek != null)
		{
			melding += " datum onderzoek " + Constants.getDateFormat().format(datumOnderzoek);
		}
		if (verwerktVerslag.getId() == null)
		{
			melding = "Nieuw. " + melding;
		}
		return melding;
	}

	@Override
	public boolean isElektronischPalgaVerslag(MammaFollowUpVerslag followUpVerslag)
	{
		return followUpVerslag.getInvoerder() == null && followUpVerslag.getVerslagContent() != null
			&& followUpVerslag.getVerslagContent().getPathologieMedischeObservatie() != null
			&& Constants.BK_TNUMMER_ELEKTRONISCH.equals(followUpVerslag.getVerslagContent().getPathologieMedischeObservatie().getTnummerLaboratorium());
	}

	@Override
	public MdlVerslag getMdlVerslagMetTNummer(PaVerslagContent verslagContent)
	{
		var spec = heeftScreeningRondeInMdlVerslag(verslagContent.getVerslag().getScreeningRonde()).and(
			heeftTNummerInPaVerslag(verslagContent.getPathologieMedischeObservatie().getTnummerLaboratorium()));
		return mdlVerslagRepository.findFirst(spec, Sort.by(SingleTableHibernateObject_.ID)).orElse(null);
	}

	@Override
	public List<PaVerslag> getPaVerslagMetTNummer(MdlVerslagContent verslagContent)
	{
		var spec = heeftScreeningRondeInPaVerslag(verslagContent.getVerslag().getScreeningRonde());
		var tnummerPathologieVerslag = verslagContent.getColoscopieMedischeObservatie().getTnummerPathologieVerslag();
		var tnummers = tnummerPathologieVerslag.stream().map(MdlTnummerPathologieVerslag::getTnummerPathologieVerslag).filter(StringUtils::isNotBlank)
			.collect(
				Collectors.toList());
		if (!tnummers.isEmpty())
		{
			spec = spec.and(heeftTNummerIn(tnummers));
			return paVerslagRepository.findAll(spec);
		}
		return new ArrayList<>();
	}

	@Override
	public DSValue getDsValue(String code, String codeSystem, String valueSetName)
	{
		return getDsValue(code, codeSystem, valueSetName, false);
	}

	@Override
	public DSValue getDsValue(String code, String codeSystem, String valueSetName, boolean ignoreCase)
	{
		var spec = heeftCode(code, ignoreCase).and(heeftCodeSystem(codeSystem)).and(heeftValueSetName(valueSetName));
		return dsValueRepository.findFirst(spec, Sort.by(AbstractHibernateObject_.ID)).orElse(null);
	}

	@Override
	public boolean heeftMdlVerslagenMetOnderzoekDatum(MdlVerslag verslag, Date onderzoekDatum)
	{
		var specification = heeftScreeningRondeInMdlVerslag(verslag.getScreeningRonde()).and(heeftVerslagStatus(VerslagStatus.AFGEROND)).and(
			heeftDatumOnderzoek(onderzoekDatum));

		if (verslag.getId() != null)
		{
			specification = specification.and(heeftNietId(verslag.getId()));
		}
		return mdlVerslagRepository.exists(specification);

	}

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW)
	public void setBerichtenOpnieuwVerwerken(List<Long> ids)
	{
		hibernateService.getHibernateSession().createQuery(
				"update " + OntvangenCdaBericht.class.getName() + " set " + OntvangenCdaBericht_.STATUS + " = :status where id in (:ids)")
			.setParameter("status", BerichtStatus.VERWERKING)
			.setParameterList("ids", ids)
			.executeUpdate();
	}
}
