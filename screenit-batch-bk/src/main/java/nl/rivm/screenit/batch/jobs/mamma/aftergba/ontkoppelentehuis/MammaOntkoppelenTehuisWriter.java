package nl.rivm.screenit.batch.jobs.mamma.aftergba.ontkoppelentehuis;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.dao.mamma.MammaBaseTehuisClientenDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.model.mamma.MammaTehuisAdres;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie;
import nl.rivm.screenit.util.AdresUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaOntkoppelenTehuisWriter extends BaseWriter<MammaTehuis>
{
	private final HibernateService hibernateService;

	private final LogService logService;

	private final MammaBaseTehuisClientenDao baseTehuisClientenDao;

	private final MammaBaseKansberekeningService baseKansberekeningService;

	@Override
	protected void write(MammaTehuis tehuis)
	{
		var crit = hibernateService.getHibernateSession().createCriteria(Client.class, "client");
		crit.createAlias("client.mammaDossier", "dossier");
		crit.add(Restrictions.eq("dossier.tehuis", tehuis));

		List<Client> gekoppeldeClienten = crit.list();
		List<Client> terechtGekoppeldeClienten = baseTehuisClientenDao.getClienten(tehuis, MammaTehuisSelectie.GEKOPPELD, null);

		List<Client> teOntkoppelenClienten = new ArrayList<>();
		for (Client client : gekoppeldeClienten)
		{
			if (!terechtGekoppeldeClienten.contains(client))
			{
				teOntkoppelenClienten.add(client);
			}
		}

		for (Client client : teOntkoppelenClienten)
		{
			MammaDossier dossier = client.getMammaDossier();
			dossier.setTehuis(null);
			hibernateService.saveOrUpdate(dossier);

			baseKansberekeningService.dossierEventHerzien(dossier);
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_TEHUIS_ONTKOPPELD, Collections.singletonList(tehuis.getStandplaats().getRegio()), client, "Tehuis: " + tehuis.getNaam());
		}

		Set<AdresWrapper> terechtGekoppeldeAdressen = new HashSet<>();
		for (Client client : terechtGekoppeldeClienten)
		{
			GbaPersoon persoon = client.getPersoon();
			terechtGekoppeldeAdressen.add(new AdresWrapper(persoon.getGbaAdres()));
			if (persoon.getTijdelijkGbaAdres() != null)
			{
				terechtGekoppeldeAdressen.add(new AdresWrapper(persoon.getTijdelijkGbaAdres()));
			}
		}

		List<MammaTehuisAdres> teVerwijderenTehuisAdressen = tehuis.getAdressen().stream()
			.filter(adres -> !adres.getLocatieVanTehuis() && !terechtGekoppeldeAdressen.contains(new AdresWrapper(adres))).collect(Collectors.toList());

		for (MammaTehuisAdres adres : teVerwijderenTehuisAdressen)
		{
			tehuis.getAdressen().remove(adres);
			hibernateService.saveOrUpdate(tehuis);
			hibernateService.delete(adres);

			logService.logGebeurtenis(LogGebeurtenis.MAMMA_TEHUIS_BEHEER, Collections.singletonList(tehuis.getStandplaats().getRegio()), null,
				String.format("Tehuis: '%s' Tehuis adres verwijderd: '%s'", tehuis.getNaam(), AdresUtil.getVolledigeAdresString(adres)), Bevolkingsonderzoek.MAMMA);
		}
	}

	@AllArgsConstructor(access = AccessLevel.PRIVATE)
	private static class AdresWrapper
	{
		private final Adres adres;

		@Override
		public boolean equals(Object o)
		{
			AdresWrapper that = (AdresWrapper) o;

			if (adres.getPostcode() != null ? !adres.getPostcode().equals(that.adres.getPostcode()) : that.adres.getPostcode() != null)
				return false;
			if (adres.getHuisnummer() != null ? !adres.getHuisnummer().equals(that.adres.getHuisnummer()) : that.adres.getHuisnummer() != null)
				return false;
			if (adres.getHuisletter() != null ? !adres.getHuisletter().equals(that.adres.getHuisletter()) : that.adres.getHuisletter() != null)
				return false;
			return adres.getHuisnummerToevoeging() != null ?
				adres.getHuisnummerToevoeging().equals(that.adres.getHuisnummerToevoeging()) :
				that.adres.getHuisnummerToevoeging() == null;
		}

		@Override
		public int hashCode()
		{
			int result = adres.getPostcode() != null ? adres.getPostcode().hashCode() : 0;
			result = 31 * result + (adres.getHuisnummer() != null ? adres.getHuisnummer().hashCode() : 0);
			result = 31 * result + (adres.getHuisletter() != null ? adres.getHuisletter().hashCode() : 0);
			result = 31 * result + (adres.getHuisnummerToevoeging() != null ? adres.getHuisnummerToevoeging().hashCode() : 0);
			return result;
		}
	}
}
