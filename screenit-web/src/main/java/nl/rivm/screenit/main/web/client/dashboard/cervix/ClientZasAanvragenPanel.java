
package nl.rivm.screenit.main.web.client.dashboard.cervix;

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
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixFactory;

import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Radio;
import org.apache.wicket.markup.html.form.RadioGroup;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientZasAanvragenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	@SpringBean
	private CervixBaseScreeningrondeService screeningrondeService;

	@SpringBean
	private CervixFactory factory;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private Date zasUitnodigingsDatum;

	private Date uitstellenTotDatum;

	private final boolean heeftAlMaxZASsenAangevraagd;

	private Boolean uitstelPeriodeNemen = false;

	public ClientZasAanvragenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);

		CervixDossier cervixDossier = client.getObject().getCervixDossier();
		CervixScreeningRonde laatsteScreeningRonde = cervixDossier.getLaatsteScreeningRonde();

		heeftAlMaxZASsenAangevraagd = screeningrondeService.heeftMaxAantalZASsenBereikt(laatsteScreeningRonde, true);
		add(new WebMarkupContainer("standaardTekst").setVisible(!heeftAlMaxZASsenAangevraagd));
		add(new WebMarkupContainer("maxZASOverschredenWaarschuwing").setVisible(heeftAlMaxZASsenAangevraagd));
		RadioGroup<Boolean> uitstelPeriodeNemen = new RadioGroup<>("uitstelPeriodeNemen", new PropertyModel<>(this, "uitstelPeriodeNemen"));
		Radio<Boolean> uitstelRadio = new Radio<>("uitstel", Model.of(Boolean.TRUE));
		uitstelPeriodeNemen.add(uitstelRadio);
		Radio<Boolean> ZASRadio = new Radio<>("ZAS", Model.of(Boolean.FALSE));
		uitstelPeriodeNemen.add(ZASRadio);
		uitstelPeriodeNemen.setOutputMarkupId(true);
		uitstelPeriodeNemen.setRequired(true);

		zasUitnodigingsDatum = currentDateSupplier.getDate();
		CervixUitstel uitstel = laatsteScreeningRonde.getUitstel();
		add(uitstelPeriodeNemen);
		if (uitstel != null && uitstel.getGeannuleerdDatum() == null)
		{
			uitstellenTotDatum = uitstel.getUitstellenTotDatum();
		}
		if (uitstellenTotDatum != null && uitstellenTotDatum.after(zasUitnodigingsDatum) && !heeftAlMaxZASsenAangevraagd)
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
	}

	private Integer maxZasAanvragen()
	{
		return screeningrondeService.getMaxAantalZASAanvragen(true);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		HashMap<ExtraOpslaanKey, Object> opslaanObjecten = new HashMap<>();
		opslaanObjecten.put(ExtraOpslaanKey.ZAS_DOOR_CLIENT_AANGEVRAAGD, true);
		opslaanObjecten.put(ExtraOpslaanKey.CERVIX_UITSTEL, uitstelPeriodeNemen);
		return opslaanObjecten;
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> meldingen = super.getOpslaanMeldingen();
		meldingen.add("Wilt u een zelfafnameset aanvragen? Klik dan op de knop ‘Ja, uitvoeren’.");
		if (uitstelPeriodeNemen)
		{
			meldingen.add(String.format("U ontvangt de zelfafnameset na %s per post.", Constants.getDateFormat().format(uitstellenTotDatum)));
		}
		else
		{
			meldingen.add("U ontvangt de zelfafnameset binnen 14 dagen per post.");
		}
		return meldingen;
	}

	@Override
	public void validate()
	{
		super.validate();

		if (heeftAlMaxZASsenAangevraagd)
		{
			error("U heeft in deze ronde al het maximaal aantal zelfafnamesets aangevraag. Via de Infolijn kun u wel nog meer aanvragen (tekst nog aanpassen).");
		}
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		if (heeftAlMaxZASsenAangevraagd)
		{
			response.render(OnDomReadyHeaderItem.forScript("$('#opslaanBtn').attr('disabled', true);"));
		}
	}
}
