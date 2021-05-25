package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import nl.rivm.screenit.dao.mamma.MammaBaseTehuisClientenDao;
import nl.rivm.screenit.main.service.mamma.MammaTehuisAdresService;
import nl.rivm.screenit.main.service.mamma.MammaTehuisService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.model.mamma.MammaTehuisAdres;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseTehuisService;
import nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie;
import nl.rivm.screenit.util.AdresUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Adres;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaTehuisAdresServiceImpl implements MammaTehuisAdresService
{

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseTehuisClientenDao baseTehuisClientenDao;

	@Autowired
	private MammaBaseTehuisService baseTehuisService;

	@Autowired
	private MammaTehuisService tehuisService;

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public void adresToevoegen(MammaTehuisAdres adres, InstellingGebruiker instellingGebruiker)
	{
		MammaTehuis tehuis = adres.getTehuis();
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_TEHUIS_BEHEER, instellingGebruiker, String.format("Adres voor tehuis '%s' aangemaakt.", tehuis.getNaam()),
			Bevolkingsonderzoek.MAMMA);
		hibernateService.saveOrUpdate(adres);
		tehuis.getAdressen().add(adres);
		baseTehuisService.saveOrUpdateTehuis(tehuis, instellingGebruiker);
	}

	@Override
	public void adresVerwijderen(MammaTehuisAdres adres, InstellingGebruiker instellingGebruiker)
	{
		MammaTehuis tehuis = adres.getTehuis();
		tehuis.getAdressen().remove(adres);
		baseTehuisService.saveOrUpdateTehuis(tehuis, instellingGebruiker);
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_TEHUIS_BEHEER, instellingGebruiker,
			String.format("Adres voor tehuis '%s' verwijderd: '%s'", tehuis.getNaam(), AdresUtil.getVolledigeAdresString(adres)), Bevolkingsonderzoek.MAMMA);
		hibernateService.delete(adres);
	}

	@Override
	public boolean isAdresAlGekoppeld(MammaTehuisAdres tehuisAdres)
	{
		MammaTehuis tehuis = tehuisAdres.getTehuis();
		boolean isAlGekoppeld = false;
		for (MammaTehuisAdres adres : tehuis.getAdressen())
		{
			isAlGekoppeld = tehuisAdres.getHuisnummer().equals(adres.getHuisnummer()) && StringUtils.equals(adres.getHuisletter(), tehuisAdres.getHuisletter())
				&& StringUtils.equals(adres.getHuisnummerToevoeging(), tehuisAdres.getHuisnummerToevoeging())
				&& StringUtils.equals(adres.getStraat(), tehuisAdres.getStraat())
				&& StringUtils.equals(adres.getPlaats(), tehuisAdres.getPlaats()) && StringUtils.equals(adres.getPostcode(), tehuisAdres.getPostcode());
		}
		return isAlGekoppeld;
	}

	@Override
	public long countClienten(MammaTehuis tehuis, MammaTehuisSelectie tehuisSelectie, Adres zoekAdres)
	{
		return baseTehuisClientenDao.countClienten(tehuis, tehuisSelectie, zoekAdres);
	}

	@Override
	public List<Client> getTehuisAdresClienten(MammaTehuis tehuis, Adres zoekAdres, int first, int count, String sortProperty, boolean isAscending)
	{
		List<Client> tehuisAdresClienten;
		if (sortProperty.equals("uitTeNodigen"))
		{
			tehuisAdresClienten = baseTehuisClientenDao.getClienten(tehuis, MammaTehuisSelectie.TEHUIS_ADRES, zoekAdres);
		}
		else
		{
			tehuisAdresClienten = baseTehuisClientenDao.getClienten(tehuis, MammaTehuisSelectie.TEHUIS_ADRES, zoekAdres, first, count, sortProperty, isAscending);
		}

		List<Client> uitTeNodigenClienten = baseTehuisClientenDao.getClienten(tehuis, MammaTehuisSelectie.UIT_TE_NODIGEN, zoekAdres);
		for (Client client : tehuisAdresClienten)
		{
			MammaDossier dossier = client.getMammaDossier();
			if (dossier.getTehuis() != null)
			{
				dossier.setUitTeNodigen(uitTeNodigenClienten.contains(client));
			}
		}

		if (sortProperty.equals("uitTeNodigen"))
		{
			Comparator<MammaDossier> uitTeNodigenComparator = Comparator.comparing(MammaDossier::getUitTeNodigen, Comparator.nullsLast(Comparator.naturalOrder()));
			tehuisAdresClienten = tehuisAdresClienten.stream()
				.map(Client::getMammaDossier)
				.sorted(isAscending ? uitTeNodigenComparator.reversed() : uitTeNodigenComparator)
				.map(MammaDossier::getClient)
				.skip(first)
				.limit(count)
				.collect(Collectors.toList());
		}
		return tehuisAdresClienten;
	}
}
