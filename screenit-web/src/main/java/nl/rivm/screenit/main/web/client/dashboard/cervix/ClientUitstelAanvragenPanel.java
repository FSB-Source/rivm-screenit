
package nl.rivm.screenit.main.web.client.dashboard.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.enums.CervixUitstelType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Radio;
import org.apache.wicket.markup.html.form.RadioGroup;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.joda.time.DateTime;

public class ClientUitstelAanvragenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private SimplePreferenceService preferenceService;

	private IModel<Client> clientModel;

	private IModel<CervixUitstel> uitstelModel;

	private IModel<Date> datumModel;

	private Integer uitstelBijZwangerschap;

	private Component uitstelTotDatumZwangerschapPanel;

	private Component uitstelTotDatumAndersPanel;

	private RadioGroup<CervixUitstelType> uitstelTypeRadio;

	public ClientUitstelAanvragenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		clientModel = client;

		uitstelBijZwangerschap = preferenceService.getInteger(PreferenceKey.UITSTEL_BIJ_ZWANGERSCHAP_CERVIX.name());

		CervixScreeningRonde ronde = client.getObject().getCervixDossier().getLaatsteScreeningRonde();
		CervixUitstel uitstel = ronde.getUitstel();
		if (uitstel == null)
		{
			uitstel = new CervixUitstel();
			uitstel.setScreeningRonde(ronde);
		}

		uitstelModel = ModelUtil.cModel(uitstel);
		datumModel = new Model<>(getDatum());

		WebMarkupContainer container = new WebMarkupContainer("container", uitstelModel);
		add(container);

		uitstelTypeRadio = new RadioGroup<>("uitstelType");
		Radio<CervixUitstelType> zwangerRadio = new Radio<>("zwanger", Model.of(CervixUitstelType.ZWANGERSCHAP));
		uitstelTypeRadio.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				ClientUitstelAanvragenPanel.this.toggleDateField(target);
			}
		});
		uitstelTypeRadio.add(zwangerRadio);
		Radio<CervixUitstelType> andersRadio = new Radio<>("anders", Model.of(CervixUitstelType.ANDERS));
		uitstelTypeRadio.add(andersRadio);

		uitstelTypeRadio.setOutputMarkupId(true);
		uitstelTypeRadio.setRequired(true);

		container.add(uitstelTypeRadio);
		toggleDateField(null);
	}

	protected void toggleDateField(AjaxRequestTarget target)
	{
		CervixUitstelType uitstelType = uitstelModel.getObject().getUitstelType();

		Component nieuwPanel = null;
		if (uitstelType != null)
		{
			switch (uitstelType)
			{
			case ZWANGERSCHAP:
				nieuwPanel = new UitstelTotDatumFragment("uitstelTotDatumZwangerschap", datumModel);
				nieuwPanel.setOutputMarkupId(true);
				if (uitstelTotDatumZwangerschapPanel == null)
				{
					uitstelTypeRadio.add(nieuwPanel);
				}
				else
				{
					uitstelTotDatumZwangerschapPanel.replaceWith(nieuwPanel);
				}
				uitstelTotDatumZwangerschapPanel = nieuwPanel;
				nieuwPanel = new EmptyPanel("uitstelTotDatumAnders");
				nieuwPanel.setOutputMarkupId(true);
				if (uitstelTotDatumAndersPanel == null)
				{
					uitstelTypeRadio.add(nieuwPanel);
				}
				else
				{
					uitstelTotDatumAndersPanel.replaceWith(nieuwPanel);
				}
				uitstelTotDatumAndersPanel = nieuwPanel;
				break;
			case ANDERS:
				nieuwPanel = new UitstelTotDatumFragment("uitstelTotDatumAnders", datumModel);
				nieuwPanel.setOutputMarkupId(true);
				if (uitstelTotDatumAndersPanel == null)
				{
					uitstelTypeRadio.add(nieuwPanel);
				}
				else
				{
					uitstelTotDatumAndersPanel.replaceWith(nieuwPanel);
				}
				uitstelTotDatumAndersPanel = nieuwPanel;
				nieuwPanel = new EmptyPanel("uitstelTotDatumZwangerschap");
				nieuwPanel.setOutputMarkupId(true);
				if (uitstelTotDatumZwangerschapPanel == null)
				{
					uitstelTypeRadio.add(nieuwPanel);
				}
				else
				{
					uitstelTotDatumZwangerschapPanel.replaceWith(nieuwPanel);
				}
				uitstelTotDatumZwangerschapPanel = nieuwPanel;
				break;
			}
		}
		else
		{
			uitstelTotDatumZwangerschapPanel = new EmptyPanel("uitstelTotDatumZwangerschap");
			uitstelTotDatumZwangerschapPanel.setOutputMarkupId(true);
			uitstelTypeRadio.add(uitstelTotDatumZwangerschapPanel);
			uitstelTotDatumAndersPanel = new EmptyPanel("uitstelTotDatumAnders");
			uitstelTotDatumAndersPanel.setOutputMarkupId(true);
			uitstelTypeRadio.add(uitstelTotDatumAndersPanel);
		}
		if (target != null)
		{
			target.add(uitstelTotDatumAndersPanel, uitstelTotDatumZwangerschapPanel);
		}
	}

	@Override
	public void validate()
	{
		super.validate();

		CervixUitstel uitstel = uitstelModel.getObject();

		if (datumModel == null || datumModel.getObject() == null)
		{
			error(getString("error.geen.uitstellenToDatum"));
		}
		else
		{
			DateTime datum = new DateTime(datumModel.getObject());
			DateTime morgen = dateSupplier.getDateTime().withTimeAtStartOfDay().plusDays(1);
			DateTime uitstellenTotDatum = new DateTime(getUitstellenTotDatum());
			CervixUitstelType uitstelType = uitstel.getUitstelType();

			if (uitstelType == CervixUitstelType.ZWANGERSCHAP && uitstellenTotDatum.isBefore(morgen))
			{
				error("De door u opgegeven datum is niet correct. Vul een geldige datum in.");
			}
			else if (uitstelType == CervixUitstelType.ZWANGERSCHAP && morgen.plusMonths(9).minusDays(1).isBefore(datum))
			{
				error(getString("error.uitstellenToDatum.meer.dan.negen.maanden"));
			}
			else if (uitstelType == CervixUitstelType.ANDERS && uitstellenTotDatum.isBefore(morgen))
			{
				error(getString("error.uitstellenToDatum.in.toekomst"));
			}
			else if (uitstel.getScreeningRonde().getDossier().getVolgendeRondeVanaf().before(uitstellenTotDatum.toDate()))
			{
				error(getString("error.uitstel.loopt.af.na.volgende.ronde"));
			}
		}
	}

	public class UitstelTotDatumFragment extends Fragment
	{

		private static final long serialVersionUID = 1L;

		public UitstelTotDatumFragment(String id, IModel<Date> model)
		{
			super(id, "uitstelTotDatumFragment", ClientUitstelAanvragenPanel.this);

			add(ComponentHelper.newDatePicker("uitstelTotDatum", model));
		}
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> meldingen = super.getOpslaanMeldingen();
		Date uitstellenTotDatum = getUitstellenTotDatum();
		String melding = "U wilt uw deelname aan het bevolkingsonderzoek baarmoederhalskanker uitstellen tot " + new SimpleDateFormat("dd-MM-yyyy").format(uitstellenTotDatum)
			+ ".\n";

		CervixUitnodiging zasUitnodiging = uitstelModel.getObject().getScreeningRonde().getLaatsteZasUitnodiging();
		if (zasUitnodiging != null && zasUitnodiging.getVerstuurdDatum() == null && zasUitnodiging.getGeannuleerdDatum() == null)
		{
			melding += "De eerder door u aangevraagde zelfafnameset wordt u nu niet meer toegezonden.\n";
			melding += "U ontvangt rond de door u aangeven datum een nieuwe uitnodiging. U kunt dan ook een nieuwe zelfafnameset aanvragen.";
		}
		else
		{
			melding += "U ontvangt rond de door u aangeven datum een nieuwe uitnodiging.";
		}
		meldingen.add(melding);
		return meldingen;
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = new HashMap<>();
		CervixUitstel uitstel = uitstelModel.getObject();
		uitstel.setUitstellenTotDatum(getUitstellenTotDatum());
		opslaanObjecten.put(ExtraOpslaanKey.CERVIX_UITSTEL, uitstel);
		return opslaanObjecten;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(uitstelModel);
		ModelUtil.nullSafeDetach(clientModel);
		ModelUtil.nullSafeDetach(datumModel);
	}

	private Date getUitstellenTotDatum()
	{
		Date datum = datumModel.getObject();
		Date uitstellenTotDatum = null;
		switch (uitstelModel.getObject().getUitstelType())
		{
		case ZWANGERSCHAP:
			uitstellenTotDatum = new DateTime(datum).plusMonths(uitstelBijZwangerschap).toDate();
			break;
		case ANDERS:
			uitstellenTotDatum = datum;
			break;
		}
		return uitstellenTotDatum;
	}

	private Date getDatum()
	{
		CervixUitstel uitstel = uitstelModel.getObject();
		Date datum = uitstel.getUitstellenTotDatum();
		if (datum != null && uitstel.getUitstelType() == CervixUitstelType.ZWANGERSCHAP)
		{
			datum = new DateTime(datum).minusMonths(uitstelBijZwangerschap).toDate();
		}
		return datum;
	}
}
