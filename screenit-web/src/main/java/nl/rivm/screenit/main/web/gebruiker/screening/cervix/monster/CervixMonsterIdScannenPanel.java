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

import nl.rivm.screenit.dao.cervix.CervixMonsterDao;
import nl.rivm.screenit.main.service.cervix.CervixUitnodigingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.service.cervix.CervixMonsterService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.behavior.FocusBehavior;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class CervixMonsterIdScannenPanel extends Panel
{
	@SpringBean
	private CervixMonsterDao monsterDao;

	@SpringBean
	private CervixUitnodigingService uitnodigingService;

	@SpringBean
	private CervixMonsterService monsterService;

	private final IModel<String> monsterId = Model.of("");

	private final TextField<String> monsterIdField;

	private final BootstrapDialog dialog;

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(JavaScriptHeaderItem.forUrl("assets/js/barcodeScanner.js"));
	}

	protected CervixMonsterIdScannenPanel(String id)
	{
		super(id);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		var form = new Form<>("form");
		add(form);

		monsterIdField = new TextField<>("monsterId", monsterId);
		monsterIdField.add(new FocusBehavior());
		monsterIdField.setRequired(true);
		form.add(monsterIdField);

		var zoekenButton = new AjaxSubmitLink("zoeken")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				CervixUitnodiging uitnodiging = null;
				var monster = monsterDao.getMonsterByMonsterId(monsterId.getObject());
				if (monster != null)
				{
					var ingelogdNamensOrganisatie = ScreenitSession.get().getInstelling();
					if (!monsterService.magInstellingMonsterInzien(ingelogdNamensOrganisatie, monster))
					{
						error(String.format(getString("laboratorium.mag.monster.niet.inzien")));
					}
					else
					{
						uitnodiging = monster.getUitnodiging();
						if (!uitnodigingService.magMonsterVerwerktWordenDoorLab(ingelogdNamensOrganisatie, uitnodiging))
						{
							error(String.format(getString("zas.mag.niet.verwerkt.worden"), ingelogdNamensOrganisatie.getNaam()));
							uitnodiging = null;
						}
					}
				}
				else
				{
					error(getString("monsterId.onbekend"));
				}
				setUitnodiging(target, uitnodiging);
			}
		};
		form.add(zoekenButton);
		form.setDefaultButton(zoekenButton);

		var handmatigeInvoerBtn = new IndicatingAjaxLink<Void>("handmatigeInvoerBtn")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target, new CervixMonsterZoekenPanel(IDialog.CONTENT_ID)
				{

					@Override
					public void setUitnodiging(AjaxRequestTarget target, CervixUitnodiging uitnodiging)
					{
						CervixMonsterIdScannenPanel.this.setUitnodiging(target, uitnodiging);
						dialog.close(target);
					}
				});
			}
		};
		form.add(handmatigeInvoerBtn);
	}

	public void leegFormulier(AjaxRequestTarget target)
	{
		monsterId.setObject("");
		monsterIdField.clearInput();
		target.add(monsterIdField);
	}

	protected abstract void setUitnodiging(AjaxRequestTarget target, CervixUitnodiging uitnodiging);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(monsterId);
	}
}
