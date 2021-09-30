package nl.rivm.screenit.main.web.gebruiker.clienten.contact.cervix;

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

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Radio;
import org.apache.wicket.markup.html.form.RadioGroup;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class CervixClientContactAanvragenNieuweZASPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	@SpringBean
	private RondeNummerService rondeNummerService;

	@SpringBean
	private CervixBaseScreeningrondeService screeningrondeBaseService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private final IModel<Client> clientModel;

	private Date uitstellenTotDatum;

	private Boolean uitstelPeriodeNemen = false;

	public CervixClientContactAanvragenNieuweZASPanel(String id, IModel<ClientContactActie> model, IModel<Client> clientModel, List<Object> extraPanelParams)
	{
		super(id, model);
		this.clientModel = clientModel;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		Date verstuurd = null;

		String extraOmschrijving = null;
		CervixDossier cervixDossier = clientModel.getObject().getCervixDossier();
		CervixScreeningRonde laatsteScreeningRonde = cervixDossier.getLaatsteScreeningRonde();
		CervixUitnodiging laatsteUitnodiging = laatsteScreeningRonde.getLaatsteUitnodiging();

		if (laatsteScreeningRonde.getLaatsteZasUitnodiging() != null)
		{
			CervixZas zas = CervixMonsterUtil.getZAS(laatsteScreeningRonde.getLaatsteZasUitnodiging().getMonster());
			if (zas != null)
			{
				verstuurd = zas.getVerstuurd();
				extraOmschrijving = "Uitnodiging-id: " + zas.getUitnodiging().getUitnodigingsId() + ", Monster-id: " + zas.getMonsterId();
				if (laatsteUitnodiging.getTemplateNaam() != null)
				{
					extraOmschrijving += ", " + laatsteUitnodiging.getTemplateNaam();
				}
			}
		}

		int rondeNr = rondeNummerService.geefRondeNummer(laatsteScreeningRonde);

		add(DateLabel.forDatePattern("datum", Model.of(verstuurd), "dd-MM-yyyy HH:mm:ss"));
		add(new EnumLabel<>("gebeurtenis", TypeGebeurtenis.BMHK_ZAS_SAMENGESTELD));
		add(new Label("extraOmschrijving", extraOmschrijving).setVisible(extraOmschrijving != null));
		add(new WebMarkupContainer("gbaMessageContainer").setVisible(!GbaStatus.INDICATIE_AANWEZIG.equals(clientModel.getObject().getGbaStatus())));
		add(new Label("maxZASOverschredenWaarschuwing", String.format(getString("message.maxZASOverschredenWaarschuwing"), maxZasAanvragenInfolijn()))
			.setVisible(screeningrondeBaseService.heeftMaxAantalZASsenBereikt(laatsteScreeningRonde, false)));

		RadioGroup<Boolean> uitstelPeriodeNemen = new RadioGroup<>("uitstelPeriodeNemen", new PropertyModel<>(this, "uitstelPeriodeNemen"));
		Radio<Boolean> uitstelRadio = new Radio<>("uitstel", Model.of(Boolean.TRUE));
		uitstelPeriodeNemen.add(uitstelRadio);
		Radio<Boolean> ZASRadio = new Radio<>("ZAS", Model.of(Boolean.FALSE));
		uitstelPeriodeNemen.add(ZASRadio);
		uitstelPeriodeNemen.setOutputMarkupId(true);
		uitstelPeriodeNemen.setRequired(true);

		Date zasUitnodigingsDatum = currentDateSupplier.getDate();
		CervixUitstel uitstel = laatsteScreeningRonde.getUitstel();
		add(uitstelPeriodeNemen);
		if (uitstel != null && uitstel.getGeannuleerdDatum() == null)
		{
			uitstellenTotDatum = uitstel.getUitstellenTotDatum();
		}
		if (uitstellenTotDatum != null && uitstellenTotDatum.after(zasUitnodigingsDatum))
		{
			uitstelPeriodeNemen.add(DateLabel.forDatePattern("uitstelDatum", Model.of(uitstellenTotDatum), "dd-MM-yyyy"));
			uitstelPeriodeNemen.add(DateLabel.forDatePattern("zasUitnodigingsDatum", Model.of(zasUitnodigingsDatum), "dd-MM-yyyy"));
		}
		else
		{
			uitstelPeriodeNemen.setVisible(false);
			uitstelPeriodeNemen.add(new Label("uitstelDatum", ""));
			uitstelPeriodeNemen.add(new Label("zasUitnodigingsDatum", ""));
		}
		add(new Label("rondeNr", rondeNr));
	}

	private Integer maxZasAanvragenInfolijn()
	{
		return screeningrondeBaseService.getMaxAantalZASAanvragen(false);
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{

		List<String> meldingen = super.getOpslaanMeldingen();
		if (uitstelPeriodeNemen)
		{
			meldingen.add(String.format("De cli\u00EBnt ontvangt de zelfafnameset na uitstelperiode (%s)", Constants.getDateFormat().format(uitstellenTotDatum)));
		}
		else
		{
			meldingen.add("De cli\u00EBnt ontvangt de zelfafnameset binnen 14 dagen per post");
		}
		CervixScreeningRonde laatsteScreeningRonde = clientModel.getObject().getCervixDossier().getLaatsteScreeningRonde();
		if (screeningrondeBaseService.isFrisseStart(laatsteScreeningRonde))
		{
			meldingen
				.add(
					"De vorige uitnodiging was nog in een CIS screeningsronde. Indien naast de ZAS ook een uitnodigingsbrief klaar staat, dan moet deze NIET tegengehouden worden.");
		}
		return meldingen;
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		HashMap<ExtraOpslaanKey, Object> opslaanObjecten = new HashMap<>();
		opslaanObjecten.put(ExtraOpslaanKey.CERVIX_UITSTEL, uitstelPeriodeNemen);
		return opslaanObjecten;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(clientModel);
	}
}
