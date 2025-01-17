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

import java.math.BigDecimal;

import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.repository.algemeen.GemeenteRepository;
import nl.rivm.screenit.repository.algemeen.PostcodeCoordinatenRepository;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.AdresUtil;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.algemeen.PostcodeCoordinatenSpecification.heeftGeenHuisnummerToevoeging;
import static nl.rivm.screenit.specification.algemeen.PostcodeCoordinatenSpecification.heeftHuisnummer;
import static nl.rivm.screenit.specification.algemeen.PostcodeCoordinatenSpecification.heeftHuisnummerToevoeging;
import static nl.rivm.screenit.specification.algemeen.PostcodeCoordinatenSpecification.heeftPostcode;

@Repository
public class CoordinatenServiceImpl implements CoordinatenService
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private PostcodeCoordinatenRepository coordinatenRepository;

	@Autowired
	private GemeenteRepository gemeenteRepository;

	@Override
	public PersoonCoordinaten getCoordinatenVanPersoon(GbaPersoon persoon)
	{
		BagAdres gbaAdres = persoon.getGbaAdres();
		TijdelijkAdres tijdelijkAdres = persoon.getTijdelijkAdres();
		PersoonCoordinaten coordinatenResults = initEnFillAdres(gbaAdres);
		if (AdresUtil.isTijdelijkAdres(persoon, dateSupplier.getLocalDate()))
		{
			coordinatenResults.vanAdres = getCoordinaten(tijdelijkAdres);
		}
		return coordinatenResults;
	}

	@Override
	public PostcodeCoordinaten getCoordinaten(Adres adres)
	{
		return getCoordinaten(adres.getPostcode(), adres.getHuisnummer(), adres.getHuisnummerToevoeging(), adres.getHuisletter());
	}

	@Override
	public PostcodeCoordinaten getCoordinaten(String postcode, Integer huisnummer, String huisnummerToevoeging, String huisletter)
	{
		var specification = heeftPostcode(postcode).and(heeftHuisnummer(huisnummer));
		if (StringUtils.isBlank(huisnummerToevoeging) && StringUtils.isBlank(huisletter))
		{
			specification = specification.and(heeftGeenHuisnummerToevoeging());
		}
		else
		{
			if (!StringUtils.isBlank(huisnummerToevoeging))
			{
				specification = specification.and(heeftHuisnummerToevoeging(huisnummerToevoeging));
			}
			else
			{
				specification = specification.and(heeftHuisnummerToevoeging(huisletter));
			}
		}
		var result = coordinatenRepository.findOne(specification).orElse(null);

		if (result == null && StringUtils.isNotBlank(huisnummerToevoeging))
		{
			result = getCoordinaten(postcode, huisnummer, null, null);
		}

		return result;
	}

	private PersoonCoordinaten initEnFillAdres(BagAdres gbaAdres)
	{
		PersoonCoordinaten coordinatenResults = new PersoonCoordinaten();
		if (gbaAdres != null)
		{
			coordinatenResults.vanAdres = gbaAdres.getPostcodeCoordinaten();
			if (coordinatenResults.vanAdres == null && gbaAdres.getGbaGemeente() != null && gbaAdres.getGbaGemeente().getLatitude() != null
				&& gbaAdres.getGbaGemeente().getLongitude() != null)
			{
				coordinatenResults.vanAdres = gbaAdres.getGbaGemeente();
			}
		}
		return coordinatenResults;
	}

	@Override
	public PersoonCoordinaten getAdresEnTijdelijkAdresCoordinatenVanPersoon(GbaPersoon persoon)
	{
		BagAdres gbaAdres = persoon.getGbaAdres();
		TijdelijkAdres tijdelijkAdres = persoon.getTijdelijkAdres();
		PersoonCoordinaten coordinatenResults = initEnFillAdres(gbaAdres);
		if (tijdelijkAdres != null)
		{

			coordinatenResults.vanTijdelijkAdres = getCoordinaten(tijdelijkAdres);
		}
		return coordinatenResults;
	}

	@Override
	@Transactional
	public void updateGemeenteCoordinaten(String gemcode, String latitude, String longitude)
	{
		var gemeente = gemeenteRepository.findOneByCode(gemcode);
		if (gemeente.isEmpty())
		{
			return;
		}
		gemeente.get().setLatitude(new BigDecimal(latitude));
		gemeente.get().setLongitude(new BigDecimal(longitude));
		gemeenteRepository.save(gemeente.get());
	}

	@Override
	@Transactional
	public void addOrUpdateCoordinaten(String postcode, String huisnr, String huisnummerToevoeging, String lat, String lon)
	{
		var bestaandeCoordinaten = getCoordinaten(postcode, Integer.valueOf(huisnr), huisnummerToevoeging, null);
		if (bestaandeCoordinaten == null)
		{
			bestaandeCoordinaten = new PostcodeCoordinaten();
			bestaandeCoordinaten.setHuisnummer(Integer.valueOf(huisnr));
			if (StringUtils.isNotBlank(huisnummerToevoeging))
			{
				bestaandeCoordinaten.setHuisnummerToevoeging(huisnummerToevoeging);
			}
			bestaandeCoordinaten.setPostcode(postcode);
		}
		bestaandeCoordinaten.setLatitude(new BigDecimal(lat));
		bestaandeCoordinaten.setLongitude(new BigDecimal(lon));
		coordinatenRepository.save(bestaandeCoordinaten);
	}
}
