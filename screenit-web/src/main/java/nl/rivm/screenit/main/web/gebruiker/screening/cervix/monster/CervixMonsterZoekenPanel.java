package nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.service.cervix.CervixUitnodigingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.TablePerClassHibernateObject;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.BSNValidator;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class CervixMonsterZoekenPanel extends Panel
{
	@SpringBean
	private LogService logService;

	@SpringBean
	private CervixUitnodigingService monstersZoekenService;

	private CervixUitnodigingenPanel uitnodigingenPanel;

	private IModel<String> monsterIdModel = Model.of("");

	private TextField<String> monsterIdField;

	private IModel<String> bsnModel = Model.of("");

	private TextField<String> bsnField;

	private IModel<Date> geboortedatumModel = Model.of();

	private DatePicker<Date> geboortedatumField;

	protected CervixMonsterZoekenPanel(String id)
	{
		super(id);
		var form = new Form<>("form");
		add(form);

		monsterIdField = new TextField<>("monsterId", monsterIdModel);
		form.add(monsterIdField.setRequired(false).setOutputMarkupId(true).add(new AttributeModifier("autofocus", "")));

		bsnField = new TextField<>("bsn", bsnModel);
		bsnField.add(new BSNValidator());
		bsnField.setRequired(false);
		bsnField.setOutputMarkupId(true);
		form.add(bsnField);

		geboortedatumField = ComponentHelper.newDatePicker("geboortedatum", geboortedatumModel);
		form.add(geboortedatumField.setRequired(false).setOutputMarkupId(true));

		uitnodigingenPanel = new CervixUitnodigingenPanel("uitnodigingenPanel")
		{

			@Override
			protected void setUitnodiging(AjaxRequestTarget target, CervixUitnodiging uitnodiging)
			{
				CervixMonsterZoekenPanel.this.setUitnodiging(target, uitnodiging);
			}
		};
		add(uitnodigingenPanel);

		AjaxSubmitLink zoekenButton = new AjaxSubmitLink("zoeken")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var monsterId = monsterIdModel.getObject();
				var bsn = bsnModel.getObject();
				var geboortedatum = geboortedatumModel.getObject();

				var uitnodigingen = new ArrayList<CervixUitnodiging>();
				var melding = monstersZoekenService.zoekMonsters(ScreenitSession.get().getInstelling(), monsterId, bsn, geboortedatum, uitnodigingen, this::getString);
				if (StringUtils.isNotBlank(melding))
				{
					error(melding);
				}
				toonUitnodigingen(target, uitnodigingen);
			}

		};
		form.add(zoekenButton);
		form.setDefaultButton(zoekenButton);

		add(new AjaxLink<Void>("close")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setUitnodiging(target, null);
			}
		});
	}

	private void toonUitnodigingen(AjaxRequestTarget target, List<CervixUitnodiging> uitnodigingen)
	{
		uitnodigingen.sort(Comparator.comparing(TablePerClassHibernateObject::getId));
		uitnodigingenPanel.replaceUitnodigingenPanel(target, uitnodigingen);
		if (!uitnodigingen.isEmpty())
		{
			Client client = uitnodigingen.get(0).getScreeningRonde().getDossier().getClient();
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_UITNODIGINGEN_INGEZIEN, ScreenitSession.get().getLoggedInAccount(), client, getString("titel"),
				Bevolkingsonderzoek.CERVIX);
		}
	}

	public abstract void setUitnodiging(AjaxRequestTarget target, CervixUitnodiging uitnodiging);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(bsnModel);
	}
}
