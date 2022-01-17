package nl.rivm.screenit.model;

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
import java.util.List;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Audited
public class BMHKLaboratorium extends Instelling
{

	private static final long serialVersionUID = 1L;

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(schema = "algemeen", name = "user_id_scanners", uniqueConstraints = @UniqueConstraint(columnNames = { "userIdScanners" }))
	private List<String> userIdScanners = new ArrayList<>();

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(schema = "algemeen", name = "instrument_names", uniqueConstraints = @UniqueConstraint(columnNames = { "instrumentNames" }))
	private List<String> instrumentNames = new ArrayList<>();

	@OneToMany(mappedBy = "bmhkLaboratorium", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<Gemeente> gemeentes = new ArrayList<>();

	@OneToMany(mappedBy = "laboratorium", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<ZASRetouradres> retouradressen = new ArrayList<>();

	@Column(length = 255)
	private String medischMircobioloog;

	@ManyToOne(fetch = FetchType.LAZY)
	private UploadDocument handtekeningMedischMircobioloog;

	@Column(length = 255)
	private String patholoog;

	@ManyToOne(fetch = FetchType.LAZY)
	private UploadDocument handtekeningPatholoog;

	@Column(length = 255)
	private String orderHost;

	@Column(length = 10)
	private String orderPort;

	@Column(length = 34, nullable = true)
	private String iban;

	@Column(length = 70, nullable = true)
	private String ibanTenaamstelling;

	@Column(length = 100, nullable = true)
	private String bmhkLabWarnMail;

	@Column(nullable = true)
	private Boolean oruBerichtenVerwerken = true;

	public List<String> getUserIdScanners()
	{
		return userIdScanners;
	}

	public void setUserIdScanners(List<String> userIdScanners)
	{
		this.userIdScanners = userIdScanners;
	}

	public List<String> getInstrumentNames()
	{
		return instrumentNames;
	}

	public void setInstrumentNames(List<String> instrumentNames)
	{
		this.instrumentNames = instrumentNames;
	}

	public List<Gemeente> getGemeentes()
	{
		return gemeentes;
	}

	public void setGemeentes(List<Gemeente> gemeentes)
	{
		this.gemeentes = gemeentes;
	}

	public String getOrderHost()
	{
		return orderHost;
	}

	public void setOrderHost(String orderHost)
	{
		this.orderHost = orderHost;
	}

	public String getOrderPort()
	{
		return orderPort;
	}

	public void setOrderPort(String orderPort)
	{
		this.orderPort = orderPort;
	}

	public List<ZASRetouradres> getRetouradressen()
	{
		return retouradressen;
	}

	public void setRetouradressen(List<ZASRetouradres> retouradressen)
	{
		this.retouradressen = retouradressen;
	}

	public String getMedischMircobioloog()
	{
		return medischMircobioloog;
	}

	public void setMedischMircobioloog(String medischMircobioloog)
	{
		this.medischMircobioloog = medischMircobioloog;
	}

	public String getPatholoog()
	{
		return patholoog;
	}

	public void setPatholoog(String patholoog)
	{
		this.patholoog = patholoog;
	}

	public UploadDocument getHandtekeningMedischMircobioloog()
	{
		return handtekeningMedischMircobioloog;
	}

	public void setHandtekeningMedischMircobioloog(UploadDocument handtekeningMedischMircobioloog)
	{
		this.handtekeningMedischMircobioloog = handtekeningMedischMircobioloog;
	}

	public UploadDocument getHandtekeningPatholoog()
	{
		return handtekeningPatholoog;
	}

	public void setHandtekeningPatholoog(UploadDocument handtekeningPatholoog)
	{
		this.handtekeningPatholoog = handtekeningPatholoog;
	}

	public String getIban()
	{
		return iban;
	}

	public void setIban(String iban)
	{
		this.iban = iban;
	}

	public String getIbanTenaamstelling()
	{
		return ibanTenaamstelling;
	}

	public void setIbanTenaamstelling(String tenaamstelling)
	{
		this.ibanTenaamstelling = tenaamstelling;
	}

	public String getBmhkLabWarnMail()
	{
		return bmhkLabWarnMail;
	}

	public void setBmhkLabWarnMail(String bmhkLabWarnMail)
	{
		this.bmhkLabWarnMail = bmhkLabWarnMail;
	}

	public Boolean getOruBerichtenVerwerken()
	{
		return oruBerichtenVerwerken;
	}

	public void setOruBerichtenVerwerken(Boolean oruBerichtenVerwerken)
	{
		this.oruBerichtenVerwerken = oruBerichtenVerwerken;
	}
}
