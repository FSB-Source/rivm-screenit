package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.text.SimpleDateFormat;
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.main.util.ExportToXslUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid.MammaSECodeValidator;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaMammograaf;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.repository.mamma.MammaOnderzoekRepository;
import nl.rivm.screenit.repository.mamma.MammaScreeningsEenheidRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftGeenBeoordelingStatusIn;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftScreeningsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.isDoorgevoerd;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.filterActief;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.filterBeoordelingsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.filterCodeContaining;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.filterNaamContaining;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.filterScreeningOrganisatie;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.heeftBeoordelingsEenheidIn;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.isActief;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaScreeningsEenheidServiceImpl implements MammaScreeningsEenheidService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private MammaScreeningsEenheidRepository screeningsEenheidRepository;

	@Autowired
	private MammaStandplaatsPeriodeService standplaatsPeriodeService;

	@Autowired
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaOnderzoekRepository onderzoekRepository;

	@Override
	public List<MammaScreeningsEenheid> getActieveScreeningsEenhedenVoorBeoordelingsEenheden(List<BeoordelingsEenheid> beoordelingsEenheden)
	{
		if (beoordelingsEenheden == null || beoordelingsEenheden.isEmpty())
		{
			return Collections.emptyList();
		}
		return screeningsEenheidRepository.findAll(isActief().and(heeftBeoordelingsEenheidIn(beoordelingsEenheden)), Sort.by(MammaScreeningsEenheid_.NAAM));
	}

	@Override
	public List<MammaScreeningsEenheid> zoekScreeningsEenheden(MammaScreeningsEenheid zoekObject, ScreeningOrganisatie regio, long first, long count, Sort sort)
	{
		return screeningsEenheidRepository.findWith(zoekScreeningsEenhedenSpecification(zoekObject, regio), q -> q.sortBy(sort).all(first, count));
	}

	@Override
	public long countScreeningsEenheden(MammaScreeningsEenheid zoekObject, ScreeningOrganisatie regio)
	{
		return screeningsEenheidRepository.count(zoekScreeningsEenhedenSpecification(zoekObject, regio));
	}

	private Specification<MammaScreeningsEenheid> zoekScreeningsEenhedenSpecification(MammaScreeningsEenheid zoekObject, ScreeningOrganisatie regio)
	{
		return filterCodeContaining(zoekObject.getCode())
			.and(filterNaamContaining(zoekObject.getNaam()))
			.and(filterScreeningOrganisatie(regio))
			.and(filterActief(zoekObject.getActief()))
			.and(filterBeoordelingsEenheid(zoekObject.getBeoordelingsEenheid()));
	}

	@Override
	public List<MammaScreeningsEenheid> getActieveScreeningsEenheden()
	{
		return screeningsEenheidRepository.findAll(isActief(), Sort.by(MammaScreeningsEenheid_.CODE));
	}

	@Override
	public List<MammaScreeningsEenheid> getActieveScreeningsEenhedenVoorBeoordelingsEenheid(BeoordelingsEenheid beoordelingEenheid)
	{
		var zoekObject = new MammaScreeningsEenheid();
		var be = hibernateService.deproxy(beoordelingEenheid);
		zoekObject.setBeoordelingsEenheid(be);
		zoekObject.setActief(true);
		return zoekScreeningsEenheden(zoekObject, null, -1, -1, Sort.by(MammaScreeningsEenheid_.NAAM));
	}

	@Override
	public String getScreeningsEenhedenNamen(BeoordelingsEenheid beoordelingsEenheid)
	{
		MammaScreeningsEenheid zoekObject = new MammaScreeningsEenheid();
		zoekObject.setActief(true);
		zoekObject.setBeoordelingsEenheid(beoordelingsEenheid);

		List<String> namen = zoekScreeningsEenheden(zoekObject, null, -1, -1, Sort.by(MammaScreeningsEenheid_.NAAM))
			.stream().map(MammaScreeningsEenheid::getNaam).collect(Collectors.toList());

		return !namen.isEmpty() ? String.join(", ", namen) : "Geen screeningseenheden gekoppeld";
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public String getGekoppeldeScreeningsEenhedenTekst(MammaStandplaats standplaats)
	{
		Date nu = currentDateSupplier.getDate();
		return standplaats.getStandplaatsRonden().stream().flatMap(standplaatsRonde -> standplaatsRonde.getStandplaatsPerioden().stream())
			.filter(standplaatsPeriode -> nu.compareTo(standplaatsPeriode.getTotEnMet()) <= 0).map(standplaatsPeriode -> standplaatsPeriode.getScreeningsEenheid().getNaam())
			.sorted()
			.distinct().collect(Collectors.joining(", "));
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public boolean saveOrUpdateSE(MammaScreeningsEenheid screeningsEenheid, InstellingGebruiker ingelogdeGebruiker)
	{
		String melding = "";
		String diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(screeningsEenheid, hibernateService.getHibernateSession());

		boolean isNieuw = screeningsEenheid.getId() == null;
		if (isNieuw)
		{
			melding += seMetCodeEnNaam(screeningsEenheid) + " aangemaakt.";
		}
		else if (diffToLatestVersion.length() > 0)
		{
			melding += seMetCodeEnNaam(screeningsEenheid) + " gewijzigd (" + diffToLatestVersion + ").";
		}

		if (StringUtils.isNotEmpty(melding))
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_SCREENINGS_EENHEID, ingelogdeGebruiker, melding, Bevolkingsonderzoek.MAMMA);
			hibernateService.saveOrUpdate(screeningsEenheid);
			baseConceptPlanningsApplicatie.sendScreeningsEenheid(screeningsEenheid, isNieuw);
			return true;
		}
		return false;
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public void deleteMammograaf(MammaMammograaf mammograaf, MammaScreeningsEenheid screeningsEenheid)
	{
		String melding = "Mammograaf " + mammograaf.getAeTitle() + " met werkstation IP-adres " + mammograaf.getWerkstationIpAdres() + " verwijderd";
		hibernateService.delete(mammograaf);
		screeningsEenheid.getMammografen().remove(mammograaf);
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_MAMMOGRAAF, ScreenitSession.get().getLoggedInInstellingGebruiker(), melding, Bevolkingsonderzoek.MAMMA);
	}

	private String seMetCodeEnNaam(MammaScreeningsEenheid screeningsEenheid)
	{
		return "SE met code '" + screeningsEenheid.getCode() + "' en naam '" + screeningsEenheid.getNaam() + "'";
	}

	@Override
	public List<MammaScreeningsEenheid> getActieveScreeningsEenhedenVoorScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return screeningsEenheidRepository.findAll(isActief().and(filterScreeningOrganisatie(screeningOrganisatie)), Sort.by(MammaScreeningsEenheid_.CODE));
	}

	@Override
	public long getAantalNietDoorgevoerdeOnderzoeken(MammaScreeningsEenheid screeningsEenheid)
	{
		return onderzoekRepository.count(heeftScreeningsEenheid(screeningsEenheid).and(isDoorgevoerd(false)));
	}

	@Override
	public long getAantalNietAfgerondeBeoordelingen(MammaScreeningsEenheid screeningsEenheid)
	{
		return onderzoekRepository.count(heeftScreeningsEenheid(screeningsEenheid)
			.and(heeftGeenBeoordelingStatusIn(MammaBeoordelingStatus.eindStatussen())));
	}

	@Override
	public String magWordenGeactiveerd(MammaScreeningsEenheid screeningsEenheid)
	{
		if (screeningsEenheid.getBeoordelingsEenheid() != null && !screeningsEenheid.getBeoordelingsEenheid().getActief())
		{
			return "screeningsEenheid.activeren.inactiveBE";
		}
		else if (!MammaSECodeValidator.isValide(screeningsEenheid.getCode()))
		{
			return "screeningsEenheid.activeren.onjuisteCode";
		}
		return "";
	}

	@Override
	public String magWordenGeinactiveerd(MammaScreeningsEenheid screeningsEenheid)
	{
		if (!standplaatsPeriodeService.getStandplaatsPeriodesSorted(screeningsEenheid).isEmpty())
		{
			return "screeningsEenheid.inactiveren.actieveStandplaatsPeriodes";
		}
		if (getAantalNietDoorgevoerdeOnderzoeken(screeningsEenheid) > 0)
		{
			return "screeningsEenheid.inactiveren.actieveOnderzoeken";
		}
		if (getAantalNietAfgerondeBeoordelingen(screeningsEenheid) > 0)
		{
			return "screeningsEenheid.inactiveren.actieveBeoordelingen";
		}
		return "";
	}

	@Override
	public String getCsvString(Iterator<? extends MammaScreeningsEenheid> screeningsEenheidIterator)
	{
		StringBuilder csvBuilder = new StringBuilder();
		SimpleDateFormat dateFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT);

		String header = "Screeningseenheid,Uitgenodigd tot en met,Uitnodigen tot en met,Vrijgegeven tot en met,Interval,Indicatie\n";
		csvBuilder.append(header);

		while (screeningsEenheidIterator.hasNext())
		{
			MammaScreeningsEenheid screeningsEenheid = screeningsEenheidIterator.next();

			csvBuilder.append(ExportToXslUtil.getCsvValue(screeningsEenheid.getNaam()));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(screeningsEenheid.getUitgenodigdTotEnMet() != null ? dateFormat.format(screeningsEenheid.getUitgenodigdTotEnMet()) : ""));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(screeningsEenheid.getUitnodigenTotEnMet() != null ? dateFormat.format(screeningsEenheid.getUitnodigenTotEnMet()) : ""));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(screeningsEenheid.getVrijgegevenTotEnMet() != null ? dateFormat.format(screeningsEenheid.getVrijgegevenTotEnMet()) : ""));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(ExportToXslUtil.getIntervalString(screeningsEenheid.getMetaDataDto().initieelIntervalMaanden)));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(screeningsEenheid.getMetaDataDto().niveau.getIndicatieTekst()));
			csvBuilder.append("\n");
		}
		return csvBuilder.toString();
	}

	@Override
	public boolean ipAdressenHebbenZelfdeGemeenschappelijkeBlokken(MammaScreeningsEenheid screeningsEenheid)
	{
		String gezamenlijkeBlokkenSeProxy = getEersteDrieIpDelen(screeningsEenheid.getIpAdres());
		for (MammaMammograaf mammograaf : screeningsEenheid.getMammografen())
		{
			String gezamenlijkeBlokkenMammograaf = getEersteDrieIpDelen(mammograaf.getWerkstationIpAdres());
			if (gezamenlijkeBlokkenMammograaf == null || !gezamenlijkeBlokkenMammograaf.equals(gezamenlijkeBlokkenSeProxy))
			{
				return false;
			}
		}
		return true;
	}

	private String getEersteDrieIpDelen(String ipAdres)
	{
		String[] ipBlokken = ipAdres.split("\\.");
		if (ipBlokken.length != 4)
		{
			return null;
		}
		return ipBlokken[0] + "." + ipBlokken[1] + "." + ipBlokken[2];
	}

	@Override
	public String valideerMinderValideAfspraakPeriodes(MammaScreeningsEenheid screeningsEenheid)
	{
		LocalTime minderValidePeriode1Vanaf = DateUtil.toLocalTime(screeningsEenheid.getMinderValidePeriode1Vanaf());
		LocalTime minderValidePeriode1TotEnMet = DateUtil.toLocalTime(screeningsEenheid.getMinderValidePeriode1TotEnMet());
		LocalTime minderValidePeriode2Vanaf = DateUtil.toLocalTime(screeningsEenheid.getMinderValidePeriode2Vanaf());
		LocalTime minderValidePeriode2TotEnMet = DateUtil.toLocalTime(screeningsEenheid.getMinderValidePeriode2TotEnMet());

		boolean periode1Ingevuld = minderValidePeriode1Vanaf != null || minderValidePeriode1TotEnMet != null;
		boolean periode2Ingevuld = minderValidePeriode2Vanaf != null || minderValidePeriode2TotEnMet != null;
		int duurMinderValideAfspraak = screeningsEenheid.getDuurMinderValideAfspraak().getMinuten();

		if (!periode1Ingevuld && !periode2Ingevuld)
		{
			return "";
		}
		if (minderValidePeriode1Vanaf == null || minderValidePeriode1TotEnMet == null)
		{
			return "mindervalide.tijdvak.beideveldengevuld";
		}
		if (minderValidePeriode1Vanaf.compareTo(minderValidePeriode1TotEnMet) >= 0)
		{
			return "mindervalide.tijdvak.vanafeerderdantotenmet";
		}
		if (ChronoUnit.MINUTES.between(minderValidePeriode1Vanaf, minderValidePeriode1TotEnMet) < duurMinderValideAfspraak)
		{
			return "mindervalide.tijdvak.minimaalparameter";
		}
		if (periode2Ingevuld)
		{
			if ((minderValidePeriode2Vanaf == null || minderValidePeriode2TotEnMet == null))
			{
				return "mindervalide.tijdvak.beideveldengevuld";
			}
			if (minderValidePeriode2Vanaf.compareTo(minderValidePeriode2TotEnMet) >= 0)
			{
				return "mindervalide.tijdvak.vanafeerderdantotenmet";
			}
			if (minderValidePeriode1TotEnMet.compareTo(minderValidePeriode2Vanaf) > 0)
			{
				return "mindervalide.tijdvak.1eerdervan2";
			}
			if (ChronoUnit.MINUTES.between(minderValidePeriode2Vanaf, minderValidePeriode2TotEnMet) < duurMinderValideAfspraak)
			{
				return "mindervalide.tijdvak.minimaalparameter";
			}
		}
		return "";
	}
}
