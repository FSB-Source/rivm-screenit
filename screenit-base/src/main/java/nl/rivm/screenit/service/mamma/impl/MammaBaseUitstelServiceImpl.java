package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class MammaBaseUitstelServiceImpl implements MammaBaseUitstelService
{

	@Lazy
	@Autowired
	MammaBaseAfspraakService baseAfspraakService;

	@Autowired
	HibernateService hibernateService;

	@Autowired
	BaseBriefService baseBriefService;

	@Autowired
	ICurrentDateSupplier currentDateSupplier;

	@Autowired
	LogService logService;

	@Autowired
	BerichtToBatchService berichtToBatchService;

	@Override
	public void saveUitstel(MammaUitstel uitstel, boolean briefAanmaken, Account account)
	{
		MammaScreeningRonde screeningRonde = uitstel.getScreeningRonde();
		MammaUitnodiging laatsteUitnodiging = screeningRonde.getLaatsteUitnodiging();
		MammaAfspraak afspraak = laatsteUitnodiging != null ? laatsteUitnodiging.getLaatsteAfspraak() : null;
		boolean wijziging = uitstel.getId() != null;

		screeningRonde.setLaatsteUitstel(uitstel);
		screeningRonde.getUitstellen().add(uitstel);

		if (afspraak != null)
		{
			baseAfspraakService.afspraakAnnuleren(afspraak, MammaAfspraakStatus.UITGESTELD, null);
		}

		uitstel.setGemaaktOp(currentDateSupplier.getDate());

		hibernateService.saveOrUpdateAll(uitstel, screeningRonde);

		if (briefAanmaken)
		{
			baseBriefService.maakMammaBrief(screeningRonde, BriefType.MAMMA_UITSTEL);
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
	public void uitstelAfzeggen(MammaUitstel uitstel, MammaUitstelGeannuleerdReden uitstelGeannuleerdReden, Date geannuleerdOp)
	{
		if (uitstel != null && uitstel.getGeannuleerdOp() == null && uitstel.getUitnodiging() == null)
		{
			uitstel.setGeannuleerdOp(geannuleerdOp);
			uitstel.setGeannuleerdReden(uitstelGeannuleerdReden);
			hibernateService.saveOrUpdate(uitstel);
		}
	}
}
