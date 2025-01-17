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

import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaBaseUitstelServiceImpl implements MammaBaseUitstelService
{

	@Lazy
	@Autowired
	private MammaBaseAfspraakService baseAfspraakService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseBriefService baseBriefService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private LogService logService;

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	private MammaBaseFactory baseFactory;

	@Override
	public void saveUitstel(MammaUitstel uitstel, boolean briefAanmaken, Account account)
	{
		MammaScreeningRonde screeningRonde = uitstel.getScreeningRonde();
		MammaUitnodiging laatsteUitnodiging = screeningRonde.getLaatsteUitnodiging();
		MammaAfspraak afspraak = laatsteUitnodiging != null ? laatsteUitnodiging.getLaatsteAfspraak() : null;
		boolean wijziging = uitstel.getId() != null;

		screeningRonde.setLaatsteUitstel(uitstel);
		screeningRonde.getUitstellen().add(uitstel);

		uitstel.setGemaaktOp(currentDateSupplier.getDate());

		hibernateService.saveOrUpdateAll(uitstel, screeningRonde);
		if (afspraak != null)
		{
			baseAfspraakService.afspraakAnnuleren(afspraak, MammaAfspraakStatus.UITGESTELD, null);
		}

		baseBriefService.setNietGegenereerdeBrievenOpTegenhouden(screeningRonde, BriefType.MAMMA_OPEN_UITNODIGINGEN);

		if (briefAanmaken)
		{
			baseBriefService.maakBvoBrief(screeningRonde, BriefType.MAMMA_UITSTEL);
		}

		StringBuilder sb = new StringBuilder();
		sb.append("Streefdatum: ");
		sb.append(new SimpleDateFormat("dd-MM-yyyy").format(uitstel.getStreefDatum()));
		sb.append(", Standplaats: ");
		sb.append(uitstel.getStandplaats().getNaam());

		LogGebeurtenis logGebeurtenis = null;
		switch (uitstel.getUitstelReden())
		{
		case ACHTERVANG_UITSTEL:
			logGebeurtenis = LogGebeurtenis.MAMMA_ACHTERVANG_UITSTEL;
			break;
		case MINDER_VALIDE_UITWIJK_UITSTEL:
			logGebeurtenis = LogGebeurtenis.MAMMA_MINDER_VALIDE_UITWIJK;
			break;
		case CLIENT_CONTACT:
			logGebeurtenis = LogGebeurtenis.MAMMA_UITSTEL;
		}

		if (wijziging)
		{
			sb.append(" (wijziging)");
		}

		logService.logGebeurtenis(logGebeurtenis, account, screeningRonde.getDossier().getClient(), sb.toString(), Bevolkingsonderzoek.MAMMA);
	}

	@Override
	public MammaUitstel getOfMaakMammaUitstel(MammaScreeningRonde screeningRonde, MammaStandplaats standplaats, Date zoekDatum)
	{
		MammaUitstel laatsteUitstel = screeningRonde.getLaatsteUitstel();

		if (laatsteUitstel != null && laatsteUitstel.getUitnodiging() == null && laatsteUitstel.getGeannuleerdOp() == null)
		{
			laatsteUitstel.setStreefDatum(zoekDatum);
			laatsteUitstel.setStandplaats(standplaats);
			return laatsteUitstel;
		}
		else
		{
			return baseFactory.maakUitstel(screeningRonde, standplaats, zoekDatum, MammaUitstelReden.CLIENT_CONTACT);
		}
	}

	@Override
	public void uitstelAfzeggen(MammaUitstel uitstel, MammaUitstelGeannuleerdReden uitstelGeannuleerdReden, Date geannuleerdOp)
	{
		if (uitstel != null && uitstel.getGeannuleerdOp() == null && uitstel.getUitnodiging() == null)
		{
			uitstel.setGeannuleerdOp(geannuleerdOp);
			uitstel.setGeannuleerdReden(uitstelGeannuleerdReden);
			hibernateService.saveOrUpdate(uitstel);
		}
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public String valideerStandplaatsPeriode(MammaStandplaatsPeriode standplaatsPeriode, LocalDate streefdatum)
	{
		if (!baseAfspraakService.valideUitstelStreefDatum(streefdatum, standplaatsPeriode))
		{
			Date vrijgegevenTotEnMet = standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet();
			if (vrijgegevenTotEnMet == null)
			{
				return "Voor deze standplaatsperiode is het niet mogelijk om uitstel aan te vragen: de 'vrijgegeven tot en met' datum is leeg.";
			}
			else
			{
				LocalDate minStreefDatum = Collections
					.max(Arrays.asList(DateUtil.toLocalDate(vrijgegevenTotEnMet).plusDays(1), DateUtil.toLocalDate(standplaatsPeriode.getVanaf())));
				String minStreefdatumText = DateUtil.LOCAL_DATE_FORMAT.format(minStreefDatum);
				String maxStreefdatumText = DateUtil.LOCAL_DATE_FORMAT.format(DateUtil.toLocalDate(standplaatsPeriode.getTotEnMet()));
				return (MessageFormat.format("De uitstel streefdatum in de gekozen standplaats moet tussen {0} en {1} liggen.", minStreefdatumText, maxStreefdatumText));
			}
		}
		return null;
	}

}
