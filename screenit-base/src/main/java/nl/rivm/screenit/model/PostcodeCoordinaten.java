
package nl.rivm.screenit.model;

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

import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "gedeeld", uniqueConstraints = @UniqueConstraint(name = "uc_postcode_coord", columnNames = { "postcode", "huisnummer", "huisnummerToevoeging" }), indexes = {
	@Index(name = "COORDINATEN_POSTCODE", columnList = "postcode"), @Index(name = "COORDINATEN_HUISNUMMER", columnList = "huisnummer"),
	@Index(name = "COORDINATEN_HUISNUMMERTOEVOEGING", columnList = "huisnummerToevoeging") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class PostcodeCoordinaten extends AbstractHibernateObject implements IGeografischeCoordinaten
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false, length = HibernateMagicNumber.L7)
	private String postcode;

	@Column(nullable = false)
	private Integer huisnummer;

	@Column(nullable = true)
	private String huisnummerToevoeging;

	@Column(precision = HibernateMagicNumber.P9, scale = HibernateMagicNumber.S6, nullable = false)
	private BigDecimal latitude;

	@Column(precision = HibernateMagicNumber.P9, scale = HibernateMagicNumber.S6, nullable = false)
	private BigDecimal longitude;

	public String getPostcode()
	{
		return postcode;
	}

	public void setPostcode(String postcode)
	{
		this.postcode = postcode;
	}

	@Override
	public BigDecimal getLatitude()
	{
		return latitude;
	}

	public void setLatitude(BigDecimal latitude)
	{
		this.latitude = latitude;
	}

	@Override
	public BigDecimal getLongitude()
	{
		return longitude;
	}

	public void setLongitude(BigDecimal longitude)
	{
		this.longitude = longitude;
	}

	public Integer getHuisnummer()
	{
		return huisnummer;
	}

	public void setHuisnummer(Integer huisnummer)
	{
		this.huisnummer = huisnummer;
	}

	public String getHuisnummerToevoeging()
	{
		return huisnummerToevoeging;
	}

	public void setHuisnummerToevoeging(String huisnummerToevoeging)
	{
		this.huisnummerToevoeging = huisnummerToevoeging;
	}

	@Override
	public String toString()
	{
		StringBuilder stringbuilder = new StringBuilder();
		stringbuilder.append("ID: ");
		stringbuilder.append(this.getId());
		stringbuilder.append(", Postcode: ");
		stringbuilder.append(postcode);
		stringbuilder.append(", Huisnummer: ");
		stringbuilder.append(huisnummer);
		stringbuilder.append(", Toevoeging: ");
		stringbuilder.append(huisnummerToevoeging);
		stringbuilder.append(", Lat: ");
		stringbuilder.append(latitude);
		stringbuilder.append(", Lon: ");
		stringbuilder.append(longitude);
		return stringbuilder.toString();
	}
}
