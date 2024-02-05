package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.ColumnDefault;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
public class ScreeningOrganisatie extends Instelling
{
	private static final long serialVersionUID = 1L;

	@OneToMany(mappedBy = "screeningOrganisatie", cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<Gemeente> gemeentes = new ArrayList<>();

	@OneToMany(mappedBy = "regio", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<ZASRetouradres> retouradressen = new ArrayList<>();

	private Integer ifobtRetourPercentage;

	private String rcmdl;

	@Column(length = 512)
	private String vertegenwoordiger;

	private String rechtbank;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "so_logo_inpakcentrum")
	private UploadDocument logo;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "so_logo_brief")
	private UploadDocument logoBrief;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "so_bestuur_sign")
	private UploadDocument bestuurSign;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "so_rcmdl_sign")
	private UploadDocument rcmdlSign;

	@ManyToOne(fetch = FetchType.LAZY)
	private UploadDocument kwaliteitslogo;

	@Column(unique = true, length = 2)
	private String regioCode;

	@Column(length = HibernateMagicNumber.L256)
	private String clientPortaalVrijeTekst;

	private String enovationKlantnummer;

	private String enovationEdiAdres;

	@Column(length = 34, nullable = true)
	private String iban;

	@ColumnDefault("10")
	private Integer afspraakDrempelBk;

	@Column(length = 70, nullable = true)
	private String ibanTenaamstelling;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY, cascade = { CascadeType.ALL })
	private RegioBvoContactGegevens regioBvoContactGegevensDk;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY, cascade = { CascadeType.ALL })
	private RegioBvoContactGegevens regioBvoContactGegevensBmhk;

	@Column(nullable = true, precision = HibernateMagicNumber.P3, scale = HibernateMagicNumber.S2)
	private BigDecimal factorEersteOnderzoekBk;

	@Column(nullable = true, precision = HibernateMagicNumber.P3, scale = HibernateMagicNumber.S2)
	private BigDecimal factorDubbeleTijdBk;

	@Column(nullable = true, precision = HibernateMagicNumber.P3, scale = HibernateMagicNumber.S2)
	private BigDecimal factorMinderValideBk;

	@Column(nullable = true)
	private Integer wekenVanTevorenUitnodigen;

	@Column(nullable = true)
	private Integer vervallenCapaciteitsreserveringDagenBk;

	@Column(nullable = true)
	private Integer minimaleDagCapaciteitMinderValideAfspraken;

	public List<Gemeente> getGemeentes()
	{
		return gemeentes;
	}

	public void setGemeentes(List<Gemeente> gemeentes)
	{
		this.gemeentes = gemeentes;
	}

	public Integer getIfobtRetourPercentage()
	{
		return ifobtRetourPercentage;
	}

	public void setIfobtRetourPercentage(Integer ifobtRetourPercentage)
	{
		this.ifobtRetourPercentage = ifobtRetourPercentage;
	}

	public String getRcmdl()
	{
		return rcmdl;
	}

	public void setRcmdl(String rcmdl)
	{
		this.rcmdl = rcmdl;
	}

	public UploadDocument getLogo()
	{
		return logo;
	}

	public void setLogo(UploadDocument logo)
	{
		this.logo = logo;
	}

	public String getRegioCode()
	{
		return regioCode;
	}

	public void setRegioCode(String regioCode)
	{
		this.regioCode = regioCode;
	}

	public String getVertegenwoordiger()
	{
		return vertegenwoordiger;
	}

	public void setVertegenwoordiger(String vertegenwoordiger)
	{
		this.vertegenwoordiger = vertegenwoordiger;
	}

	public String getRechtbank()
	{
		return rechtbank;
	}

	public void setRechtbank(String rechtbank)
	{
		this.rechtbank = rechtbank;
	}

	public UploadDocument getBestuurSign()
	{
		return bestuurSign;
	}

	public void setBestuurSign(UploadDocument bestuurSign)
	{
		this.bestuurSign = bestuurSign;
	}

	public UploadDocument getRcmdlSign()
	{
		return rcmdlSign;
	}

	public void setRcmdlSign(UploadDocument rcmdlSign)
	{
		this.rcmdlSign = rcmdlSign;
	}

	public UploadDocument getKwaliteitslogo()
	{
		return kwaliteitslogo;
	}

	public void setKwaliteitslogo(UploadDocument kwaliteitslogo)
	{
		this.kwaliteitslogo = kwaliteitslogo;
	}

	public String getClientPortaalVrijeTekst()
	{
		return clientPortaalVrijeTekst;
	}

	public void setClientPortaalVrijeTekst(String clientPortaalVrijeTekst)
	{
		this.clientPortaalVrijeTekst = clientPortaalVrijeTekst;
	}

	public String getEnovationKlantnummer()
	{
		return enovationKlantnummer;
	}

	public void setEnovationKlantnummer(String enovationKlantnummer)
	{
		this.enovationKlantnummer = enovationKlantnummer;
	}

	public String getEnovationEdiAdres()
	{
		return enovationEdiAdres;
	}

	public void setEnovationEdiAdres(String enovationEdiAdres)
	{
		this.enovationEdiAdres = enovationEdiAdres;
	}

	public UploadDocument getLogoBrief()
	{
		return logoBrief;
	}

	public void setLogoBrief(UploadDocument logoBrief)
	{
		this.logoBrief = logoBrief;
	}

	public List<ZASRetouradres> getRetouradressen()
	{
		return retouradressen;
	}

	public void setRetouradressen(List<ZASRetouradres> retouradressen)
	{
		this.retouradressen = retouradressen;
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

	public Integer getAfspraakDrempelBk()
	{
		return afspraakDrempelBk;
	}

	public void setAfspraakDrempelBk(Integer afspraakDrempelBk)
	{
		this.afspraakDrempelBk = afspraakDrempelBk;
	}

	public RegioBvoContactGegevens getRegioBvoContactGegevensDk()
	{
		return regioBvoContactGegevensDk;
	}

	public void setRegioBvoContactGegevensDk(RegioBvoContactGegevens regioBvoContactGegevensDk)
	{
		this.regioBvoContactGegevensDk = regioBvoContactGegevensDk;
	}

	public RegioBvoContactGegevens getRegioBvoContactGegevensBmhk()
	{
		return regioBvoContactGegevensBmhk;
	}

	public void setRegioBvoContactGegevensBmhk(RegioBvoContactGegevens regioBvoContactGegevensBmhk)
	{
		this.regioBvoContactGegevensBmhk = regioBvoContactGegevensBmhk;
	}

	public BigDecimal getFactorMinderValideBk()
	{
		return factorMinderValideBk;
	}

	public void setFactorMinderValideBk(BigDecimal factorMinderValide)
	{
		this.factorMinderValideBk = factorMinderValide;
	}

	public BigDecimal getFactorDubbeleTijdBk()
	{
		return factorDubbeleTijdBk;
	}

	public void setFactorDubbeleTijdBk(BigDecimal factorDubbeleTijd)
	{
		this.factorDubbeleTijdBk = factorDubbeleTijd;
	}

	public BigDecimal getFactorEersteOnderzoekBk()
	{
		return factorEersteOnderzoekBk;
	}

	public void setFactorEersteOnderzoekBk(BigDecimal factorFirstTimer)
	{
		this.factorEersteOnderzoekBk = factorFirstTimer;
	}

	public Integer getWekenVanTevorenUitnodigen()
	{
		return wekenVanTevorenUitnodigen;
	}

	public void setWekenVanTevorenUitnodigen(Integer wekenVanTevorenUitnodigen)
	{
		this.wekenVanTevorenUitnodigen = wekenVanTevorenUitnodigen;
	}

	public Integer getMinimaleDagCapaciteitMinderValideAfspraken()
	{
		return minimaleDagCapaciteitMinderValideAfspraken;
	}

	public void setMinimaleDagCapaciteitMinderValideAfspraken(Integer minimaleDagCapaciteitMinderValideAfspraken)
	{
		this.minimaleDagCapaciteitMinderValideAfspraken = minimaleDagCapaciteitMinderValideAfspraken;
	}

	public Integer getVervallenCapaciteitsreserveringDagenBk()
	{
		return vervallenCapaciteitsreserveringDagenBk;
	}

	public void setVervallenCapaciteitsreserveringDagenBk(Integer vervallenCapaciteitsreserveringDagen)
	{
		this.vervallenCapaciteitsreserveringDagenBk = vervallenCapaciteitsreserveringDagen;
	}
}
