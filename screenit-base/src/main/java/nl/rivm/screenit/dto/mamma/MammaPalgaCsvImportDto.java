package nl.rivm.screenit.dto.mamma;

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

import java.io.Serializable;
import java.util.Date;

public class MammaPalgaCsvImportDto implements Serializable
{
	private long pseudoId;

	private Date geboortejaar;

	private Date aanvangVerrichting;

	private Date eindeVerrichting;

	private Date datumOntvangstMateriaal;

	private Date datumEersteAutorisatie;

	private String verkrijgingswijze;

	private String zijdigheid;

	private String locatie;

	private String LocatieInUren;

	private String oestrogeenReceptorStatus;

	private String progesteronReceptorStatus;

	private String her2Status;

	private String bClassificatie;

	private String cClassificatie;

	private String maligniteitsgraad;

	private String ptnm;

	private String stadiering;

	private String versieProtocol;

	private boolean isReedsAangeleverd = false;

	private boolean isPatid3 = false;

	private int regelNummer;

	private boolean heeftFout = false;

	public long getPseudoId()
	{
		return pseudoId;
	}

	public void setPseudoId(long pseudoId)
	{
		this.pseudoId = pseudoId;
	}

	public Date getGeboortejaar()
	{
		return geboortejaar;
	}

	public void setGeboortejaar(Date geboortejaar)
	{
		this.geboortejaar = geboortejaar;
	}

	public Date getAanvangVerrichting()
	{
		return aanvangVerrichting;
	}

	public void setAanvangVerrichting(Date aanvangVerrichting)
	{
		this.aanvangVerrichting = aanvangVerrichting;
	}

	public Date getEindeVerrichting()
	{
		return eindeVerrichting;
	}

	public void setEindeVerrichting(Date eindeVerrichting)
	{
		this.eindeVerrichting = eindeVerrichting;
	}

	public Date getDatumOntvangstMateriaal()
	{
		return datumOntvangstMateriaal;
	}

	public void setDatumOntvangstMateriaal(Date datumOntvangstMateriaal)
	{
		this.datumOntvangstMateriaal = datumOntvangstMateriaal;
	}

	public Date getDatumEersteAutorisatie()
	{
		return datumEersteAutorisatie;
	}

	public void setDatumEersteAutorisatie(Date datumEersteAutorisatie)
	{
		this.datumEersteAutorisatie = datumEersteAutorisatie;
	}

	public String getVerkrijgingswijze()
	{
		return verkrijgingswijze;
	}

	public void setVerkrijgingswijze(String verkrijgingswijze)
	{
		this.verkrijgingswijze = verkrijgingswijze;
	}

	public String getZijdigheid()
	{
		return zijdigheid;
	}

	public void setZijdigheid(String zijdigheid)
	{
		this.zijdigheid = zijdigheid;
	}

	public String getLocatie()
	{
		return locatie;
	}

	public void setLocatie(String locatie)
	{
		this.locatie = locatie;
	}

	public String getLocatieInUren()
	{
		return LocatieInUren;
	}

	public void setLocatieInUren(String locatieInUren)
	{
		LocatieInUren = locatieInUren;
	}

	public String getOestrogeenReceptorStatus()
	{
		return oestrogeenReceptorStatus;
	}

	public void setOestrogeenReceptorStatus(String oestrogeenReceptorStatus)
	{
		this.oestrogeenReceptorStatus = oestrogeenReceptorStatus;
	}

	public String getProgesteronReceptorStatus()
	{
		return progesteronReceptorStatus;
	}

	public void setProgesteronReceptorStatus(String progesteronReceptorStatus)
	{
		this.progesteronReceptorStatus = progesteronReceptorStatus;
	}

	public String getHer2Status()
	{
		return her2Status;
	}

	public void setHer2Status(String her2Status)
	{
		this.her2Status = her2Status;
	}

	public String getbClassificatie()
	{
		return bClassificatie;
	}

	public void setbClassificatie(String bClassificatie)
	{
		this.bClassificatie = bClassificatie;
	}

	public String getcClassificatie()
	{
		return cClassificatie;
	}

	public void setcClassificatie(String cClassificatie)
	{
		this.cClassificatie = cClassificatie;
	}

	public String getMaligniteitsgraad()
	{
		return maligniteitsgraad;
	}

	public void setMaligniteitsgraad(String maligniteitsgraad)
	{
		this.maligniteitsgraad = maligniteitsgraad;
	}

	public String getPtnm()
	{
		return ptnm;
	}

	public void setPtnm(String ptnm)
	{
		this.ptnm = ptnm;
	}

	public String getStadiering()
	{
		return stadiering;
	}

	public void setStadiering(String stadiering)
	{
		this.stadiering = stadiering;
	}

	public String getVersieProtocol()
	{
		return versieProtocol;
	}

	public void setVersieProtocol(String versieProtocol)
	{
		this.versieProtocol = versieProtocol;
	}

	public boolean isReedsAangeleverd()
	{
		return isReedsAangeleverd;
	}

	public void setReedsAangeleverd(boolean reedsAangeleverd)
	{
		isReedsAangeleverd = reedsAangeleverd;
	}

	public boolean isPatid3()
	{
		return isPatid3;
	}

	public void setPatid3(boolean isPatid3)
	{
		this.isPatid3 = isPatid3;
	}

	public int getRegelNummer()
	{
		return regelNummer;
	}

	public void setRegelNummer(int regelNummer)
	{
		this.regelNummer = regelNummer;
	}

	public boolean heeftFout()
	{
		return heeftFout;
	}

	public void setHeeftFout(boolean heeftFout)
	{
		this.heeftFout = heeftFout;
	}
}
