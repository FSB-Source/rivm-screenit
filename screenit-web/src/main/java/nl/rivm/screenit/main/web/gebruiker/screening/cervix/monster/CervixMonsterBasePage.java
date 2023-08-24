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
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.CervixBarcodeAfdrukkenBasePage;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class CervixMonsterBasePage extends CervixBarcodeAfdrukkenBasePage
{

	private CervixMonsterIdScannenPanel monsterIdScannenPanel;

	private Panel uitnodigingPanel;

	private final CervixUitnodigingenPanel vorigeMonstersPanel;

	public CervixMonsterBasePage()
	{
		addMonsterIdScannenPanel();
		uitnodigingPanel = getUitnodigingPanel(null, null);
		add(uitnodigingPanel);

		vorigeMonstersPanel = new CervixUitnodigingenPanel("vorigeMonsters")
		{

			@Override
			protected void setUitnodiging(AjaxRequestTarget target, CervixUitnodiging uitnodiging)
			{
				CervixMonsterBasePage.this.replaceUitnodigingPanel(target, uitnodiging);
			}
		};
		add(vorigeMonstersPanel);
	}

	private void addMonsterIdScannenPanel()
	{
		monsterIdScannenPanel = new CervixMonsterIdScannenPanel("monsterIdScannenPanel")
		{
			@Override
			protected void setUitnodiging(AjaxRequestTarget target, CervixUitnodiging uitnodiging)
			{
				CervixMonsterBasePage.this.replaceUitnodigingPanel(target, uitnodiging);
			}
		};
		add(monsterIdScannenPanel);
	}

	private Panel getUitnodigingPanel(AjaxRequestTarget target, CervixUitnodiging uitnodiging)
	{
		Panel newUitnodigingPanel = null;
		if (target != null && uitnodiging != null)
		{
			if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
			{
				newUitnodigingPanel = new CervixUitnodigingUitstrijkjePanel(this, "uitnodigingPanel", (CervixUitstrijkje) HibernateHelper.deproxy(uitnodiging.getMonster()))
				{
					@Override
					protected boolean ontvangstMonster()
					{
						return CervixMonsterBasePage.this.ontvangstMonster();
					}

					@Override
					protected void focusMonsterId(AjaxRequestTarget target)
					{
						monsterIdScannenPanel.leegFormulier(target);
					}
				};
			}
			else
			{
				CervixUitnodigingZasPanel uitnodigingZasPanel = new CervixUitnodigingZasPanel(this, "uitnodigingPanel",
					(CervixZas) HibernateHelper.deproxy(uitnodiging.getMonster()))
				{
					@Override
					protected boolean ontvangstMonster()
					{
						return CervixMonsterBasePage.this.ontvangstMonster();
					}

					@Override
					protected void focusMonsterId(AjaxRequestTarget target)
					{
						monsterIdScannenPanel.leegFormulier(target);
					}

					@Override
					protected void onInitialize()
					{
						boolean nuInboeken = nuInboeken();
						super.onInitialize();
						if (nuInboeken)
						{
							showBarcode(null, false);
						}

					}
				};
				newUitnodigingPanel = uitnodigingZasPanel;
			}
		}
		else
		{
			newUitnodigingPanel = new EmptyPanel("uitnodigingPanel");
			newUitnodigingPanel.setOutputMarkupPlaceholderTag(true);
		}
		return newUitnodigingPanel;
	}

	private void voegUitnodigingToe(AjaxRequestTarget target, CervixUitnodiging uitnodiging)
	{
		final IModel<?> zoekObjectModel = ScreenitSession.get().getZoekObject(this.getClass());
		final List<CervixUitnodiging> result = new ArrayList<>();
		if (zoekObjectModel != null)
		{
			final List<CervixUitnodiging> oudeLijst = zonderDezeUitnodiging((List<CervixUitnodiging>) zoekObjectModel.getObject(), uitnodiging);
			if (oudeLijst.size() <= 5)
			{
				result.addAll(oudeLijst);
			}
			else
			{
				result.addAll(oudeLijst.subList(0, 5));
			}
		}
		result.add(0, uitnodiging);
		if (result.size() > 1)
		{
			vorigeMonstersPanel.replaceUitnodigingenPanel(target, result.subList(1, result.size()));
		}
		ScreenitSession.get().setZoekObject(this.getClass(), Model.of(result));
	}

	private List<CervixUitnodiging> zonderDezeUitnodiging(List<CervixUitnodiging> lijst, CervixUitnodiging uitnodiging)
	{
		return lijst.stream()
			.filter(u -> !u.getId().equals(uitnodiging.getId()))
			.collect(Collectors.toList());
	}

	private void replaceUitnodigingPanel(AjaxRequestTarget target, CervixUitnodiging uitnodiging)
	{
		if (uitnodiging != null)
		{
			voegUitnodigingToe(target, uitnodiging);
		}
		Panel newUitnodigingPanel = getUitnodigingPanel(target, uitnodiging);
		uitnodigingPanel.replaceWith(newUitnodigingPanel);
		uitnodigingPanel = newUitnodigingPanel;
		target.add(uitnodigingPanel);

		monsterIdScannenPanel.leegFormulier(target);
	}

	protected abstract boolean ontvangstMonster();
}
